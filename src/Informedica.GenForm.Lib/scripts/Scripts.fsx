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
open Informedica.GenForm.Lib

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


    
// Testing

RuleFinder.createFilter (Some 12.) (Some 10.) None (Some 9504) "" "" "or"
|> RuleFinder.find
|> RuleFinder.convertToResult

GenPresProduct.getAssortment ()
|> Array.collect (fun gpp ->
    gpp.GenericProducts
    |> Array.map (fun gp -> gp.Id)
)
|> Array.distinct

let test = 
    { Dto.dto with
        BirthYear = 2017
        BirthMonth = 3
        BirthDay = 2
        WeightKg = 10.
        LengthCm = 70.
        GPK = "15334"
        MultipleUnit = "mcg"
        Route = "iv"
    }

Dto.findRules test

