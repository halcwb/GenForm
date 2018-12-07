namespace Informedica.GenForm.Lib


module Dto =

    open Aether  

    open Informedica.GenForm.Lib

    open System
    open MathNet.Numerics

    open Informedica.GenUtils.Lib
    open Informedica.GenUtils.Lib.BCL
    open Informedica.GenUnits.Lib    

    open GStand

    module Dosage = DoseRule.Dosage
    module DoseRange = DoseRule.DoseRange
    
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
            LengthCm : float
            BSAInM2 : float
            Gender : string
            BirthWeightGram : float
            GestAgeWeeks : int
            GestAgeDays : int
            GPK : int
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
            IsRate : Boolean
            RateUnit : string
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
            LengthCm = 0.
            BSAInM2 = 0.
            Gender = ""
            BirthWeightGram = 0.
            GestAgeWeeks = 0
            GestAgeDays = 0
            GPK = 0
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
            IsRate = false
            RateUnit = ""
            Rules = [||]
            Text = ""
        }


    let loadGenForm () =
        GPP.load false
        DR.load ()
        ATC.load ()


    let find (dto : Dto) =
        let rte = dto.Route |> Mapping.mapRoute Mapping.AppMap Mapping.GStandMap

        let gpps =
            let ps = dto.GPK |> GPP.findByGPK 
            if ps |> Array.length = 0 then
                GPP.filter true dto.Generic dto.Shape rte
            else ps
            |> Array.toList

        match gpps with
        | [gpp] -> 
            let gpk, lbl, conc, unt, tps = 
                let gp = 
                    match gpp.GenericProducts |> Seq.tryFind (fun p -> p.Id = dto.GPK) with
                    | Some gp -> gp |> Some
                    | None -> 
                        if gpp.GenericProducts |> Seq.length = 1 then gpp.GenericProducts |> Seq.head |> Some
                        else None
                
                match gp with
                | Some gp -> 
                    let conc, unt =
                        match gp.Substances |> Seq.tryFind (fun s -> s.SubstanceName |> String.equalsCapInsens gpp.Name) with
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

                    gp.Id, gp.Label, conc, unt, tps
                
                | None -> 0, "", 0., "", ""

            gpk, gpp.Name, gpp.Shape, lbl, conc, unt, tps
        | _ -> 
            printfn "Could not find product %s %s %s with GPK: %i" dto.Generic dto.Shape dto.Route dto.GPK
            0, "", "", "", 0., "", "" 



    let processDto2 (dto : Dto) =

        let u =
            dto.MultipleUnit |> ValueUnit.unitFromAppString 

        let ru = 
            dto.RateUnit |> ValueUnit.Units.fromString

        let rte = dto.Route |> Mapping.mapRoute Mapping.AppMap Mapping.GStandMap
        
        let dto =
            if dto.BSAInM2 > 0. then dto
            else 
                if dto.LengthCm > 0. && dto.WeightKg > 0. then
                    {
                        dto with 
                            BSAInM2 =
                                // (w / (l  ** 2.)) |> Some
                                dto.WeightKg / (dto.LengthCm ** 2.)
                    }
                else
                    dto

        let freqsToStr (fr : Dosage.Frequency) =
            fr.Frequencies
            |> List.map (fun f ->
                f
                |> ValueUnit.create (ValueUnit.createCombiUnit (ValueUnit.Units.Count.times, ValueUnit.OpPer, fr.TimeUnit))
                |> ValueUnit.freqToValueUnitString
                |> (fun s -> printfn "%s" s; s)
                |> Mapping.mapFreq Mapping.ValueUnitMap Mapping.AppMap
            )
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

        let gpk, gen, shp, lbl, conc, unt, tps = find dto

        let rs = 
            let su = 
                if dto.MultipleUnit = "" then None
                else
                    dto.MultipleUnit
                    |> ValueUnit.unitFromAppString

            let tu = 
                if dto.RateUnit = "" then None
                else
                    dto.RateUnit
                    |> ValueUnit.unitFromAppString

            let cfg = 
                { UseAll = true ; IsRate = dto.IsRate ; SubstanceUnit = su ; TimeUnit = tu }

            GStand.createDoseRules 
                cfg 
                (Some dto.AgeInMo) 
                (Some dto.WeightKg)
                (Some dto.BSAInM2) 
                (Some gpk) 
                gen 
                shp 
                rte
        
        if rs |> Seq.length <> 1 then 
            printfn "found %i rules" (rs |> Seq.length)
            dto
        else
            let r = rs |> Seq.head

            let rules =
                let ids =
                    r.IndicationsDosages 
                    |> Seq.filter (fun d -> 
                        d.Indications 
                        |> List.exists (fun s ->
                            if dto.Indication |> String.isNullOrWhiteSpace then 
                                s = "Algemeen"
                            else 
                                s = dto.Indication
                        )
                    )

                if ids |> Seq.length <> 1 then
                    printfn "wrong ids count: %A" ids
                    []
                else
                    let id = ids |> Seq.head
                    if id.RouteDosages |> Seq.length <> 1 then 
                        printfn "wrong rds count: %A" id.RouteDosages                        
                        []
                    else
                        let rd = id.RouteDosages |> Seq.head
                        if rd.ShapeDosages |> Seq.length <> 1 then 
                            printfn "wrong sds count: %A" rd.ShapeDosages                        
                            []
                        else
                            let sd = rd.ShapeDosages |> Seq.head

                            printfn "found %i patient dosages" (sd.PatientDosages |> Seq.length)

                            sd.PatientDosages
                            |> List.collect (fun pd ->
                                pd.SubstanceDosages
                                |> List.filter (fun sd -> 
                                    printfn "filtering %s = %s" sd.Name gen
                                    sd.Name |> String.equalsCapInsens gen
                                )
                            )
                            |> List.groupBy (fun sd ->
                                sd 
                                |> Dosage.Optics.getFrequencyTimeUnit
                            )
                            |> List.map (fun (_, sds) ->

                                sds
                                |> List.fold (fun acc d ->
                                    //printfn "folding %s" (d |> Dosage.toString false)
                                    {
                                        acc with

                                            Frequency = 
                                                d.TotalDosage
                                                |> snd
                                                |> freqsToStr

                                            MinTotalDose = d |> getValue Dosage.Optics.inclMinNormTotalDosagePrism
                                            MaxTotalDose = d |> getValue Dosage.Optics.exclMaxNormTotalDosagePrism

                                            MinTotalDosePerKg = d |> getValue Dosage.Optics.inclMinNormWeightTotalDosagePrism
                                            MaxTotalDosePerKg = d |> getValue Dosage.Optics.exclMaxNormWeightTotalDosagePrism

                                            MinTotalDosePerM2 = d |> getValue Dosage.Optics.inclMinNormBSATotalDosagePrism
                                            MaxTotalDosePerM2 = d |> getValue Dosage.Optics.exclMaxNormBSATotalDosagePrism

                                            MaxPerDose   = 
                                                if acc.MaxPerDose = 0. then
                                                    let d1 = d |> getValue Dosage.Optics.exclMaxNormSingleDosagePrism
                                                    let d2 = d |> getValue Dosage.Optics.exclMaxNormStartDosagePrism
                                                    if d1 = 0. then d2 else d1
                                                else acc.MaxPerDose
                                            MaxPerDosePerKg =
                                                if acc.MaxPerDosePerKg = 0. then
                                                    let d1 = d |> getValue Dosage.Optics.exclMaxNormWeightSingleDosagePrism
                                                    let d2 = d |> getValue Dosage.Optics.exclMaxNormWeightStartDosagePrism
                                                    if d1 = 0. then d2 else d1
                                                else acc.MaxPerDosePerKg
                                            MaxPerDosePerM2 =
                                                if acc.MaxTotalDosePerM2 = 0. then
                                                    let d1 = d |> getValue Dosage.Optics.exclMaxNormBSASingleDosagePrism
                                                    let d2 = d |> getValue Dosage.Optics.exclMaxNormBSAStartDosagePrism
                                                    if d1 = 0. then d2 else d1
                                                else acc.MaxTotalDosePerM2 
                                    }
                                ) rule
                            )
                |> (fun rules ->
                    match rules |> List.tryFind (fun r -> r.Frequency = "") with
                    | None -> rules
                    | Some noFreq ->
                        rules
                        |> List.filter (fun r -> r.Frequency <> "")
                        |> List.map (fun r ->
                            {
                                r with
                                    MaxPerDose = noFreq.MaxPerDose
                                    MaxPerDosePerKg = noFreq.MaxPerDosePerKg
                                    MaxPerDosePerM2 = noFreq.MaxPerDosePerM2
                            }
                        )
                )
            
            {
                dto with
                    ATC = r.ATC
                    TherapyGroup = r.ATCTherapyGroup
                    TherapySubGroup = r.ATCTherapySubGroup
                    GPK = if dto.GPK <> gpk then gpk else dto.GPK
                    Generic = gen
                    TradeProduct = tps
                    Shape = shp
                    Label = lbl
                    Concentration = conc
                    ConcentrationUnit = 
                        unt 
                        |> Mapping.mapUnit Mapping.GStandMap Mapping.AppMap
                    Multiple =
                        if dto.Multiple = 0. then conc
                        else dto.Multiple
                    MultipleUnit = 
                        if dto.MultipleUnit = "" then 
                            unt 
                            |> Mapping.mapUnit Mapping.GStandMap Mapping.AppMap
                        else dto.MultipleUnit
                    Rules = rules |> List.toArray
                    Text = 
                        r 
                        |> (fun dr -> match u  with | Some u -> dr |> DoseRule.convertSubstanceUnitTo gen u | None -> dr)
                        |> (fun dr -> match ru with | Some u -> dr |> DoseRule.convertRateUnitTo gen u | None -> dr)
                        |> DoseRule.toString false
                        |> Markdown.toHtml
            }
