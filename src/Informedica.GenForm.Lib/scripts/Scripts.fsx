#load "references.fsx"

#time

open System

let pwd = Environment.GetEnvironmentVariable("HOME")
Environment.CurrentDirectory <- pwd + "/Development/GenFormService/" //__SOURCE_DIRECTORY__ + "/../../../"

open MathNet.Numerics

open Informedica.GenUtils.Lib
open Informedica.GenUtils.Lib.BCL
open Informedica.GenUnits.Lib
open Informedica.GenUnits.Lib.Api
open Informedica.GenProduct.Lib
open Informedica.GenForm.Lib

FilePath.formulary |> (fun p -> printfn "%s" p; p) |> File.exists
let printResult m r = printf m; printfn " %A" r; r


    
// Testing

RuleFinder.createFilter None None None (Some 121967) "" "" "iv"
|> RuleFinder.find
|> RuleFinder.convertToResult
|> Option.bind (fun rs ->
    rs.Doses
    |> Array.groupBy (fun d ->
        d.Freq.Time
    ) |> Some

)

GenPresProduct.getAssortment ()
|> Array.collect (fun gpp ->
    gpp.GenericProducts
    |> Array.map (fun gp -> gp.Id)
)
|> Array.distinct

let test = 
    { Dto.dto with
        BirthYear = 2018
        BirthMonth = 4
        BirthDay = 10
        WeightKg = 1.6
        LengthCm = 50.
        GPK = "3689"
        MultipleUnit = "mg"
        Route = "iv"
    }

Dto.findRules test


"1 Times[Count]/1 Day[Time]"
|> Api.fromString

GenPresProduct.getAssortment ()
|> Array.collect (fun gpp ->
    gpp.Route
)
|> Array.distinct
|> Array.sort
|> Array.iter (printfn "%s")


let getProduct gen shp =
    let gpps =
        GenPresProduct.getAssortment ()
        |> Array.filter (fun gpp ->
            gpp.Name  |> String.equalsCapInsens gen &&
            gpp.Shape |> String.equalsCapInsens shp
        )
    if gpps |> Array.length <> 1 then None
    else
        let gpp = gpps.[0]
        {
            Product.empty with
                Name = gpp.Name 
                Shape = gpp.Shape
                Unit = gpp.Unit
        }
        |> Some

DoseRule.Patient.empty
|> DoseRule.Patient.setMaxAgeMonths (Some 25.)
|> DoseRule.Patient.setMaxWeightKg (Some 10.)
|> DoseRule.Patient.toString