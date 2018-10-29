
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


open Informedica.GenProduct.Lib





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


    [<CLIMutable>]
    type Dto = 
        {
            BirthYear : int
            BirthMonth : int
            BirthDay : int
            WeightKg : float
            BirthWeightGram : float
            LengthCm : float
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

            NormDose : float
            MinDose : float
            MaxDose : float
            MaxTotal : float
            MaxPerDose : float

            NormDosePerKg : float
            MinDosePerKg : float
            MaxDosePerKg : float
            MaxTotalPerKg : float
            MaxPerDosePerKg : float

            NormDosePerM2 : float
            MinDosePerM2 : float
            MaxDosePerM2 : float
            MaxTotalPerM2 : float
            MaxPerDosePerM2 : float
        }

    let rule =
        {
            Frequency = ""
            NormDose = 0.
            MinDose = 0.
            MaxDose = 0.
            MaxTotal = 0.
            MaxPerDose = 0.
            NormDosePerKg = 0.
            MinDosePerKg = 0.
            MaxDosePerKg = 0.
            MaxTotalPerKg = 0.
            MaxPerDosePerKg = 0.
            NormDosePerM2 = 0.
            MinDosePerM2 = 0.
            MaxDosePerM2 = 0.
            MaxTotalPerM2 = 0.
            MaxPerDosePerM2 = 0.
        }



    let dto = 
        {
            BirthYear = 0
            BirthMonth = 0
            BirthDay = 0
            WeightKg = 0.
            BirthWeightGram = 0.
            LengthCm = 0.
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

    
    let toDto age weight bsa gpk route =

        let getValue prism d =
            d
            |> (Optic.get prism)
            |> (fun vu ->
                match vu with
                | Some vu ->
                    vu 
                    |> ValueUnit.getValue
                    |> BigRational.ToDouble
                | None -> 0.
            )


        let gen, shp, lbl, conc, unt =
            match gpk |> GPP.findByGPK |> Array.toList with
            | [gpp] -> 
                let lbl, conc, unt = 
                    match gpp.GenericProducts |> Seq.tryFind (fun gp -> gp.Id = gpk) with
                    | Some gp -> 
                        let conc, unt =
                            match gp.Substances |> Seq.tryFind (fun s -> s.SubstanceName = gpp.Name) with
                            | Some s -> s.SubstanceQuantity, s.SubstanceUnit
                            | None -> 0., ""
                        gp.Label, conc, unt
                    | None -> "", 0., ""

                gpp.Name, gpp.Shape, lbl, conc, unt
            | _ -> "", "", "", 0., ""

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
                            if sd.PatientDosages |> Seq.length <> 1 then []
                            else
                                let pd = sd.PatientDosages |> Seq.head
                                pd.SubstanceDosages
                                |> List.filter (fun sd ->
                                    sd.Name |> String.equalsCapInsens gen
                                )
                                |> List.map (fun d ->
                                    {
                                        rule with
                                            Frequency = 
                                                d.Frequencies
                                                |> DoseRule.Dosage.freqsToStr

                                            MinDose = d |> getValue Dosage.Optics.inclMinNormTotalDosagePrism
                                            MaxDose = d |> getValue Dosage.Optics.exclMaxNormTotalDosagePrism

                                            MinDosePerKg = d |> getValue Dosage.Optics.inclMinNormWeightTotalDosagePrism
                                            MaxDosePerKg = d |> getValue Dosage.Optics.exclMaxNormWeightTotalDosagePrism
                                    }
                                )
            
            {
                dto with
                    GPK = gpk |> string
                    ATC = r.ATC
                    TherapyGroup = r.ATCTherapyGroup
                    TherapySubGroup = r.ATCTherapySubGroup
                    Generic = r.Generic
                    TradeProduct = ""
                    Shape = shp
                    Label = lbl
                    Concentration = conc
                    ConcentrationUnit = unt
                    Multiple = 0.
                    MultipleUnit = ""
                    Route = route
                    Rules = rules |> List.toArray
                    Text = r |> DoseRule.toString false
            }


GenPresProduct.filter false "paracetamol" "" ""
|> Seq.collect (fun gpp ->
    gpp.GenericProducts
    |> Seq.collect (fun gp ->
        gp.Route
        |> Seq.map (fun r ->
            Dto.toDto (Some 22.) (Some 12.) None gp.Id r
        )
    )
)

Dto.toDto (Some 0.) (Some 1.5) None 3689 "intraveneus"

