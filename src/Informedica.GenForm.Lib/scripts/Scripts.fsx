#load "references.fsx"

#time

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ + "/../../../"

open MathNet.Numerics

open Informedica.GenUtils.Lib
open Informedica.GenUtils.Lib.BCL
open Informedica.GenUnits.Lib
open Informedica.GenUnits.Lib.Api
open Informedica.GenProduct.Lib

FilePath.formulary |> (fun p -> printfn "%s" p; p) |> File.exists


// type GenFormItem =
//     {
//         GenPresProduct : GenPresProduct.GenPresProduct
//         ResultProduct : ProductRange.ProductRange
//         Rules : RuleFinder.RuleResult
//     }

// let create gpp rp rs = 
//     {
//         GenPresProduct = gpp
//         ResultProduct = rp
//         Rules = rs

//     }


// let get age wght bsa rt (rp : ProductRange.ProductRange) =
//     if rp.GPK |> Option.isNone then None
//     else
//         match [| rp |] |>  GenPresProduct.get with
//         | [| gpp |] -> 
//             RuleFinder.createFilter age wght bsa rp.GPK "" "" (if rt = "" then rp.Route else rt)
//             |> RuleFinder.find
//             |> RuleFinder.convertToResult
//             |> create gpp rp 
//             |> Some
//         | _ -> None


// ProductRange.data ()
// |> Array.filter (fun pr -> 
//     pr.Generic  |> String.equalsCapInsens "paracetamol"
//     && pr.Route |> String.contains "iv"
// )
// //|> (fun pr -> printfn "%A" pr; pr)
// |> Array.map (get (Some 4.) (Some 5.6) None "iv")
// |> Array.iter (printfn "%A")
// |> ignore


// type Frequency = 
//     | NoFrequency
//     | FrequencyValue of FrequencyValue
// and FrequencyValue = { Value : BigRational; Time : CombiUnit.CombiUnit}

// type DoseType =
//     | PerDose of Adjust
//     | PerTime of Adjust
// and Adjust = None | PerKg | PerM2

// let createFrequency freq time unit =
//     match Unit.Units.fromString unit "Time" with
//     | Some u' ->
//         let v = freq |> BigRational.fromFloat
//         let t = 
//             time
//             |> BigRational.fromFloat
//             |> CombiUnit.withUnit u'
//         { Value = v; Time = t } |> FrequencyValue
//     | None -> NoFrequency


// let freqToUnitValue = function
//     | NoFrequency -> 
//         (1N.toVU Api.Count_Times)
//     | FrequencyValue (fv) ->
//         (fv.Value.toVU Api.Count_Times) / (ValueUnit.create 1N fv.Time)

let printResult m r = printf m; printfn " %A" r; r

module Dto =


    // Private m_BD As Date
    // Private m_WT As Double
    // Private m_BW As Double
    // Private m_LE As Double
    // Private m_GS As String
    // Private m_GW As Integer
    // Private m_GD As Integer
    // Private m_GPK As String
    // Private m_ATC As String
    // Private m_TherapieGroep As String
    // Private m_TherapieSubgroep As String
    // Private m_Generiek As String
    // Private m_Product As String
    // Private m_Vorm As String
    // Private m_Etiket As String
    // Private m_Sterkte As Double
    // Private m_SterkteEenheid As String
    // Private m_DeelDose As Double
    // Private m_DoseEenheid As String
    // Private m_Route As String
    // Private m_Indicatie As String
    // Private m_Freq As String
    // Private m_PerDose As Boolean
    // Private m_PerKg As Boolean
    // Private m_PerM2 As Boolean
    // Private m_NormDose As Double
    // Private m_MinDose As Double
    // Private m_MaxDose As Double
    // Private m_AbsDose As Double
    // Delimiter string = ||

    // BSA = m_WT ^ 0.425 * m_LE ^ 0.725 * 0.007184


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
            Frequency : string
            PerDose : bool
            PerKg : bool
            PerM2 : bool
            NormDose : float
            MinDose : float
            MaxDose : float
            AbsMaxTotal : float
            AbsMaxPerDose : float
            Rules : string
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
            Frequency = ""
            PerDose = false
            PerKg = false
            PerM2 = false
            NormDose = 0.
            MinDose = 0.
            MaxDose = 0.
            AbsMaxTotal = 0.
            AbsMaxPerDose = 0.
            Rules = ""
        }

    type Mapping = FormMap | GStandMap | PedMap

    let mapping path m1 m2 s =
        let i1, i2 =
            match m1, m2 with
            | FormMap, GStandMap -> 0, 1
            | GStandMap, FormMap -> 1, 0
            | _ -> 0, 0

        File.readAllLines path
        |> Array.skip 1
        |> Array.map (String.splitAt ';')
        |> Array.filter (fun x -> x.[i1] |> String.equalsCapInsens s)
        |> Array.fold (fun acc xs ->  
            if acc = "" then xs.[i2]
            else acc + "||" + xs.[i2]
        ) ""


    let unitMapping = mapping (Environment.CurrentDirectory + "/" + FilePath.data + "/formulary/UnitMapping.csv")

    let frequencyMapping = mapping (Environment.CurrentDirectory + "/" + FilePath.data + "/formulary/FrequencyMapping.csv")


    let toDto (dto : Dto) (rs : RuleFinder.RuleResult) =
        // get only doses with mappable frequencies
        let doses = 
            rs.Doses
            |> Array.filter (fun d ->
                (string d.Freq.Frequency + " " + d.Freq.Time)
                |> frequencyMapping GStandMap FormMap 
                |> ((<>) "")
            ) 
            |> printResult "doses: "

        match doses with
        | [||] ->
            dto
        | _ ->

            // AbsMax per dose
            let absPer = 
                let dosesOnce =
                    doses
                    |> Array.filter (fun d ->
                        d.Freq.Frequency = 1.
                    )
                dosesOnce
                |> Array.map (fun d -> d.NormDose)
                |> Array.append (dosesOnce |> Array.map (fun d -> d.AbsDose))
                |> DoseRule.foldMinMax
                |> (fun minmax ->
                    match minmax.Min, minmax.Max with
                    | _, Some max -> max
                    | _ -> 0.
                )

            // normal dose
            let normDose, perKg, perM2 =
                doses
                |> Array.map (fun d -> d.NormDose)
                |> DoseRule.foldMinMax
                |> (fun minmax -> 
                    match minmax.Min, minmax.Max with
                    | Some min, Some max when min = max -> (min, false, false)
                    | _                                 -> (0.,  false, false)
                )

            // normal dose per Kg
            let normDose, perKg, perM2 =
                doses
                |> Array.map (fun d -> d.NormKg)
                |> DoseRule.foldMinMax
                |> (fun minmax -> 
                    match minmax.Min, minmax.Max with
                    | Some min, Some max when min = max -> (min,      true,  false)
                    | _                                 -> (normDose, perKg, perM2)
                )

            // normal dose per m2
            let normDose, perKg, perM2 =
                doses
                |> Array.map (fun d -> d.NormM2)
                |> DoseRule.foldMinMax
                |> (fun minmax -> 
                    match minmax.Min, minmax.Max with
                    | Some min, Some max when min = max -> (min,     false,  true)
                    | _                                 -> (normDose, perKg, perM2)
                )

            // max dose
            let maxDose, perKg, perM2 =
                doses
                |> Array.map (fun d -> d.NormDose)
                |> Array.append (doses |> Array.map (fun d -> d.AbsDose))
                |> DoseRule.foldMinMax
                |> (fun minmax -> 
                    match minmax.Min, minmax.Max with
                    | None, Some max                     -> (max, false, false)
                    | Some min, Some max when min <> max -> (max, false, false)
                    | _                                  -> (0.,  perKg, perM2)
                )

            // normal dose
            let absMax = maxDose

            // max dose per kg
            let maxDose, perKg, perM2 =
                doses
                |> Array.map (fun d -> d.NormKg)
                |> Array.append (doses |> Array.map (fun d -> d.AbsKg))
                |> DoseRule.foldMinMax
                |> (fun minmax -> 
                    match minmax.Min, minmax.Max with
                    | None, Some max                     -> (max, true,  false)
                    | Some min, Some max when min <> max -> (max, true,  false)
                    | _                                  -> (maxDose, perKg, perKg)
                )

            // max dose per m2
            let maxDose, perKg, perM2 =
                doses
                |> Array.map (fun d -> d.NormM2)
                |> Array.append (doses |> Array.map (fun d -> d.AbsM2))
                |> DoseRule.foldMinMax
                |> (fun minmax -> 
                    match minmax.Min, minmax.Max with
                    | None, Some max                     -> (max, false, true)
                    | Some min, Some max when min <> max -> (max, false, true)
                    | _                                  -> (maxDose, perKg, perM2)
                )


            // min dose
            let minDose, perKg, perM2 =
                doses
                |> Array.map (fun d -> d.NormDose)
                |> Array.append (doses |> Array.map (fun d -> d.AbsDose))
                |> DoseRule.foldMinMax
                |> (fun minmax -> 
                    match minmax.Min, minmax.Max with
                    | Some min, None                     -> (min, false, false)
                    | Some min, Some max when min <> max -> (min, false, false)
                    | _                                  -> (0.,  perKg, perM2)
                )

            // min dose per kg
            let minDose, perKg, perM2 =
                doses
                |> Array.map (fun d -> d.NormKg)
                |> Array.append (doses |> Array.map (fun d -> d.AbsKg))
                |> DoseRule.foldMinMax
                |> (fun minmax -> 
                    match minmax.Min, minmax.Max with
                    | Some min, None                     -> (min, true, false)
                    | Some min, Some max when min <> max -> (min, true, false)
                    | _                                  -> (minDose, perKg, perM2)
                )

            // min dose per m2
            let minDose, perKg, perM2 =
                doses
                |> Array.map (fun d -> d.NormM2)
                |> Array.append (doses |> Array.map (fun d -> d.AbsM2))
                |> DoseRule.foldMinMax
                |> (fun minmax -> 
                    match minmax.Min, minmax.Max with
                    | Some min, None                     -> (min, false, true)
                    | Some min, Some max when min <> max -> (min, false, true)
                    | _                                  -> (minDose,  perKg, perM2)
                )


            { dto with
                Frequency = 
                    doses
                    |> Array.map (fun d ->
                        (string d.Freq.Frequency + " " + d.Freq.Time)
                        |> frequencyMapping GStandMap FormMap 
                    )
                    |> String.concat "||"

                // make sure that the multiple unit is mappable and the
                // same for each dose
                MultipleUnit = 
                    doses
                    |> Array.fold (fun acc d ->
                        if unitMapping GStandMap FormMap d.Unit = acc then acc
                        else ""
                    ) dto.MultipleUnit
            
                PerKg = perKg
                PerM2 = perM2
                NormDose = normDose
                MinDose = minDose
                MaxDose = maxDose
                AbsMaxTotal = absMax
                AbsMaxPerDose  = absPer
            }

        |> (fun dto' ->
            { dto' with
                Rules = 
                    rs.DoseRules 
                    |> String.concat "||"
            }
        )

    let findRules (dto : Dto) =
        let age = 
            let dt = DateTime(dto.BirthYear, dto.BirthMonth, dto.BirthDay)
            ((DateTime.Now - dt).Days |> float) / 30.
            |> (fun n -> printfn "%f" n; n)
            |> Some

        let bsa =
            if dto.LengthCm > 0. && dto.WeightKg > 0. then
                (dto.WeightKg ** 0.425) * (dto.LengthCm ** 0.725) * 0.007184
                |> Some
            else None

        let wght = 
            if dto.WeightKg > 0. then dto.WeightKg |> Some else None

        let gpk = if dto.GPK = "" then None else dto.GPK |> Int32.parse |> Some
        
        RuleFinder.createFilter age wght bsa gpk "" "" dto.Route
        |> RuleFinder.find
        |> RuleFinder.convertToResult
        |> toDto dto
    
    // Testing

    RuleFinder.createFilter (Some 12.) (Some 10.) None (Some 9504) "" "" "or"
    |> RuleFinder.find
    |> RuleFinder.convertToResult

    let test = 
        { dto with
            BirthYear = 2017
            BirthMonth = 3
            BirthDay = 2
            WeightKg = 10.
            LengthCm = 70.
            GPK = "100331"
            MultipleUnit = "mg"
            Route = "or"
        }

    findRules test