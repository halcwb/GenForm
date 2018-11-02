
#I __SOURCE_DIRECTORY__

#load "./../../../.paket/load/netstandard2.0/main.group.fsx"

#r "./../../Informedica.GenUtils.Lib/bin/Debug/netstandard2.0/Informedica.GenUtils.Lib.dll"
#r "./../../Informedica.GenUnits.Lib/bin/Debug/netstandard2.0/Informedica.GenUnits.Lib.dll"
#r "./../../Informedica.GenProduct.Lib/bin/Debug/netstandard2.0/Informedica.GenProduct.Lib.dll"

#r "netstandard"


#time


open System

let pwd = Environment.GetEnvironmentVariable("HOME")
Environment.CurrentDirectory <- 
    if pwd |> String.IsNullOrWhiteSpace then 
        __SOURCE_DIRECTORY__ + "/../../../"

    else 
        pwd + "/Development/GenForm/" //__SOURCE_DIRECTORY__ + "/../../../"


#load "./../Utils/String.fs" 
#load "./../Utils/List.fs" 
#load "./../Mapping.fs" 
#load "./../ValueUnit.fs" 
#load "./../MinMax.fs" 
#load "./../Route.fs" 
#load "./../Product.fs" 
#load "./../Patient.fs" 
#load "./../DoseRule.fs" 
#load "./../GStand.fs" 



module Dto =

    open Aether  

    open Informedica.GenForm.Lib

    open System
    open MathNet.Numerics

    open Informedica.GenUtils.Lib
    open Informedica.GenUtils.Lib.BCL
    open Informedica.GenUnits.Lib    

    open GStand
    open Informedica.GenUtils.Lib.BCL

    module Dosage = DoseRule.Dosage
    
    module RF = Informedica.GenProduct.Lib.RuleFinder 
    module DR = Informedica.GenProduct.Lib.DoseRule
    module GPP = Informedica.GenProduct.Lib.GenPresProduct
    module GP = Informedica.GenProduct.Lib.GenericProduct
    module FP = Informedica.GenProduct.Lib.FilePath
    module ATC = Informedica.GenProduct.Lib.ATCGroup


    let (>?) = MinMax.valueOptLT
    let (<?) = MinMax.valueOptST


    [<CLIMutable>]
    type Dto = 
        {
            AgeInMo : float
            WeightKg : float
            BirthWeightGram : float
            BSAInM2 : float
            Gender : string
            GestAgeWeeks : int
            GestAgeDays : int
            GPK : string
            ATC : string
            TherapyGroup : string
            TherapySubGroup : string
            Generic : string
            TradeProduct : string
            Shape : string
            Label : string
            Concentration : float
            ConcentrationUnit : string
            Multiple : float
            MultipleUnit : string
            Route : string
            Indication : string
            Rules : Rule []
            Text : string
        }
    and Rule =
        {
            Frequency : string

            NormTotalDose : float
            MinTotalDose : float
            MaxTotalDose : float
            MaxPerDose : float

            NormTotalDosePerKg : float
            MinTotalDosePerKg : float
            MaxTotalDosePerKg : float
            MaxPerDosePerKg : float

            NormTotalDosePerM2 : float
            MinTotalDosePerM2 : float
            MaxTotalDosePerM2 : float
            MaxPerDosePerM2 : float
        }

    let rule =
        {
            Frequency = ""
            NormTotalDose = 0.
            MinTotalDose = 0.
            MaxTotalDose = 0.
            MaxPerDose = 0.
            NormTotalDosePerKg = 0.
            MinTotalDosePerKg = 0.
            MaxTotalDosePerKg = 0.
            MaxPerDosePerKg = 0.
            NormTotalDosePerM2 = 0.
            MinTotalDosePerM2 = 0.
            MaxTotalDosePerM2 = 0.
            MaxPerDosePerM2 = 0.
        }



    let dto = 
        {
            AgeInMo = 0.
            WeightKg = 0.
            BirthWeightGram = 0.
            BSAInM2 = 0.
            Gender = ""
            GestAgeWeeks = 0
            GestAgeDays = 0
            GPK = ""
            ATC = ""
            TherapyGroup = ""
            TherapySubGroup = ""
            Generic = ""
            TradeProduct = ""
            Shape = ""
            Label = ""
            Concentration = 0.
            ConcentrationUnit = ""
            Multiple = 0.
            MultipleUnit = ""
            Route = ""
            Indication = ""
            Rules = [||]
            Text = ""
        }

    type Mapping = FormMap | GStandMap | PedMap  | StandMap


    let mapping path m1 m2 s =
        let i1, i2 =
            match m1, m2 with
            | FormMap,   GStandMap -> 0, 1
            | GStandMap, FormMap   -> 1, 0
            | FormMap,   StandMap  -> 0, 3
            | GStandMap, StandMap  -> 1, 3
            | StandMap,  FormMap   -> 3, 0
            | StandMap,  GStandMap -> 1, 3
            | _ -> 0, 0

        let map =
            File.readAllLines path
            |> Array.skip 1
            |> Array.map (String.splitAt ';')
        if i1 = 0 && i2 = 0 || (i1 > map.Length || i2 > map.Length) then ""
        else
            map
            |> Array.filter (fun x -> x.[i1] |> String.equalsCapInsens s)
            |> Array.fold (fun acc xs ->  
                if acc = "" then xs.[i2]
                else acc + "||" + xs.[i2]
            ) ""


    let unitMapping = mapping (Environment.CurrentDirectory + "/" + FP.data + "/formulary/UnitMapping.csv")

    
    let frequencyMapping = mapping (Environment.CurrentDirectory + "/" + FP.data + "/formulary/FrequencyMapping.csv")


    let loadGenForm () =
        GPP.load false
        DR.load ()
        ATC.load ()

    
    let toDto age weight bsa gend gpk route =

        let freqsToStr (fr : Dosage.Frequency) =
            fr.Frequencies
            |> List.map (fun f ->
                Dosage.createFrequency [f] fr.TimeUnit fr.MinimalInterval
            )
            |> List.map Dosage.freqsToStr
            |> String.concat "||"

        let getValue prism d =
            d
            |> (Optic.get prism)
            |> (fun vu ->
                match vu with
                | Some vu ->
                    vu 
                    |> ValueUnit.getValue
                    |> BigRational.ToDouble
                    |> Double.fixPrecision 2
                | None -> 0.
            )


        let gen, shp, lbl, conc, unt, tps =
            match gpk |> GPP.findByGPK |> Array.toList with
            | [gpp] -> 
                let lbl, conc, unt, tps = 
                    match gpp.GenericProducts |> Seq.tryFind (fun gp -> gp.Id = gpk) with
                    | Some gp -> 
                        let conc, unt =
                            match gp.Substances |> Seq.tryFind (fun s -> s.SubstanceName = gpp.Name) with
                            | Some s -> s.SubstanceQuantity, s.SubstanceUnit
                            | None -> 0., ""

                        let tps =
                            gp.PrescriptionProducts
                            |> Array.fold (fun acc pp ->
                                pp.TradeProducts
                                |> Array.map (fun tp -> tp.Label)
                                |> Array.toList
                                |> List.append acc
                            ) []
                            |> String.concat "||"
                        gp.Label, conc, unt, tps
                    | None -> "", 0., "", ""

                gpp.Name, gpp.Shape, lbl, conc, unt, tps
            | _ -> "", "", "", 0., "", ""

        let rs = GStand.createDoseRules false age weight bsa (Some gpk) gen shp route
        
        if rs |> Seq.length <> 1 then dto
        else
            let r = rs |> Seq.head

            let rules =
                let ids =
                    r.IndicationsDosages 
                    |> Seq.filter (fun d -> d.Indications |> List.exists (String.equalsCapInsens "Algemeen"))

                if ids |> Seq.length <> 1 then []
                else
                    let id = ids |> Seq.head
                    if id.RouteDosages |> Seq.length <> 1 then []
                    else
                        let rd = id.RouteDosages |> Seq.head
                        if rd.ShapeDosages |> Seq.length <> 1 then []
                        else
                            let sd = rd.ShapeDosages |> Seq.head

                            let (sds, pds) =
                                sd.PatientDosages
                                |> List.partition (fun pd ->
                                    pd.SubstanceDosages
                                    |> List.filter (fun sd ->
                                        sd.Name = gen
                                    )
                                    |> List.exists (fun sd -> sd.Frequencies.TimeUnit = ValueUnit.NoUnit)
                                )

                            if pds |> List.length = 0 then [] 
                            else
                                pds
                                |> List.fold (fun (acc : PatientDosage) pd ->
                                    if acc.Patient.Age.Min >? pd.Patient.Age.Min ||
                                       acc.Patient.Age.Max >? pd.Patient.Age.Max ||
                                       acc.Patient.Weight.Min >? pd.Patient.Weight.Min ||
                                       acc.Patient.Weight.Max <? pd.Patient.Weight.Max ||
                                       acc.Patient.BSA.Min >? pd.Patient.BSA.Min ||
                                       acc.Patient.BSA.Max <? pd.Patient.BSA.Max then acc
                                    else pd
                                ) (sd.PatientDosages |> List.head)
                                |> (fun pd ->
                                    pd.SubstanceDosages 
                                    |> List.filter (fun sd ->
                                        sd.Name = gen
                                    )
                                    |> List.map (fun d ->
                                        {
                                            rule with
                                                Frequency = 
                                                    d.Frequencies
                                                    |> freqsToStr

                                                MinTotalDose = d |> getValue Dosage.Optics.inclMinNormTotalDosagePrism
                                                MaxTotalDose = d |> getValue Dosage.Optics.exclMaxNormTotalDosagePrism
                                                MaxPerDose   = 
                                                    let d1 = d |> getValue Dosage.Optics.exclMaxNormSingleDosagePrism
                                                    let d2 = d |> getValue Dosage.Optics.exclMaxNormStartDosagePrism
                                                    if d1 = 0. then d2 else d1

                                                MinTotalDosePerKg = d |> getValue Dosage.Optics.inclMinNormWeightTotalDosagePrism
                                                MaxTotalDosePerKg = d |> getValue Dosage.Optics.exclMaxNormWeightTotalDosagePrism
                                                MaxPerDosePerKg =
                                                    let d1 = d |> getValue Dosage.Optics.exclMaxNormWeightSingleDosagePrism
                                                    let d2 = d |> getValue Dosage.Optics.exclMaxNormWeightStartDosagePrism
                                                    if d1 = 0. then d2 else d1

                                                MinTotalDosePerM2 = d |> getValue Dosage.Optics.inclMinNormBSATotalDosagePrism
                                                MaxTotalDosePerM2 = d |> getValue Dosage.Optics.exclMaxNormBSATotalDosagePrism
                                                MaxPerDosePerM2 =
                                                    let d1 = d |> getValue Dosage.Optics.exclMaxNormBSASingleDosagePrism
                                                    let d2 = d |> getValue Dosage.Optics.exclMaxNormBSAStartDosagePrism
                                                    if d1 = 0. then d2 else d1
                                        } 
                                        |> (fun r ->
                                            let sds = 
                                                sds 
                                                |> List.collect (fun d -> 
                                                    d.SubstanceDosages
                                                    |> List.filter (fun d -> d.Name = gen)
                                                )
                                            if sds |> List.length <> 1 then r
                                            else
                                                sds
                                                |> List.head
                                                |> (fun d ->
                                                    {
                                                        r with
                                                            MaxPerDose   = 
                                                                let d1 = d |> getValue Dosage.Optics.exclMaxNormSingleDosagePrism
                                                                let d2 = d |> getValue Dosage.Optics.exclMaxNormStartDosagePrism
                                                                if d1 = 0. then d2 else d1
                                                            MaxPerDosePerKg =
                                                                let d1 = d |> getValue Dosage.Optics.exclMaxNormWeightSingleDosagePrism
                                                                let d2 = d |> getValue Dosage.Optics.exclMaxNormWeightStartDosagePrism
                                                                if d1 = 0. then d2 else d1
                                                            MaxPerDosePerM2 =
                                                                let d1 = d |> getValue Dosage.Optics.exclMaxNormBSASingleDosagePrism
                                                                let d2 = d |> getValue Dosage.Optics.exclMaxNormBSAStartDosagePrism
                                                                if d1 = 0. then d2 else d1
                                                    }
                                                )
                                        )
                                    )
                                )

            
            {
                dto with
                    AgeInMo = 
                        if age |> Option.isSome then age |> Option.get 
                        else 0.
                    WeightKg = 
                        if weight |> Option.isSome then weight |> Option.get 
                        else 0.
                    BSAInM2 = 
                        if bsa |> Option.isSome then bsa |> Option.get
                        else 0.
                    Gender = gend
                    GPK = gpk |> string
                    ATC = r.ATC
                    TherapyGroup = r.ATCTherapyGroup
                    TherapySubGroup = r.ATCTherapySubGroup
                    Generic = r.Generic
                    TradeProduct = tps
                    Shape = shp
                    Label = lbl
                    Concentration = conc
                    ConcentrationUnit = unt |> unitMapping GStandMap FormMap
                    Multiple = 0.
                    MultipleUnit = ""
                    Route = route
                    Rules = rules |> List.toArray
                    Text = r |> DoseRule.toString false
            }




open Informedica.GenProduct.Lib


GenPresProduct.filter false "ondansetron" "" "oraal"
|> Seq.collect (fun gpp ->
    gpp.GenericProducts
    |> Seq.collect (fun gp ->
        gp.Route
        |> Seq.map (fun r ->
            Dto.toDto (Some 22.) (Some 12.) None "" gp.Id r
        )
    )
)


Dto.toDto (Some 0.) (Some 1.5) None "" 3689 "intraveneus"


