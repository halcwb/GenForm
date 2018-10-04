#load "references.fsx"

#time

open System

let pwd = Environment.GetEnvironmentVariable("HOME")
Environment.CurrentDirectory <- 
    if pwd |> String.IsNullOrWhiteSpace then 
        __SOURCE_DIRECTORY__ + "/../../../"

    else 
        pwd + "/Development/GenForm/" //__SOURCE_DIRECTORY__ + "/../../../"

open MathNet.Numerics

open Informedica.GenUtils.Lib
open Informedica.GenUtils.Lib.BCL
open Informedica.GenUnits.Lib
open Informedica.GenUnits.Lib.Api
open Informedica.GenProduct.Lib
open Informedica.GenForm.Lib

FilePath.formulary |> (fun p -> printfn "%s" p; p) |> File.exists
let printResult m r = printf m; printfn " %A" r; r
    

GenPresProduct.getAssortment ()
|> Array.filter (fun gpp ->
    gpp.GenericProducts
    |> Array.exists (fun gp ->
        gp.Substances |> Array.length = 2
    )
)


DoseRule.GStand.map (GenPresProduct.filter "paracetamol" "" "")
|> List.map (fun dr -> dr |> DoseRule.toString)
|> List.iter (printfn "%s")


"per dag" |> ValueUnit.unitFromString Mapping.GStandMap

GenPresProduct.getAssortment ()
|> Array.filter (fun gpp ->
    gpp.GenericProducts
    |> Array.exists (fun gp ->
        gp.Substances |> Array.length = 2
    )
)
|> Array.map (fun gpp -> gpp.Name)
|> Array.distinct

"216 maand[Time]" 
|> ValueUnit.fromString
|> ValueUnit.convertTo ValueUnit.Units.Time.year

GenPresProduct.filter "gentamicine" "" ""
|> Array.collect(fun gpp ->
    gpp.GenericProducts 
    |> Array.collect (fun gp ->
        gp.Substances |> Array.map (fun s -> s.SubstanceName, s.SubstanceQuantity, s.SubstanceUnit)
    )
)
|> Array.distinct


DoseRule.get ()
|> Array.map (fun dr ->
    dr.Freq
)
|> Array.distinct
|> Array.sortBy (fun f -> f.Time, f.Frequency)
|> Array.map (DoseRule.GStand.mapFreq >> ValueUnit.toStringPrec 1)
|> Array.iter (printfn "%s")


// Print the doserule text for trimethoprim/sulfamethoxazol
DoseRule.GStand.map (GenPresProduct.filter "TRIMETHOPRIM/SULFAMETHOXAZOL" "" "")
|> List.map Informedica.GenForm.Lib.DoseRule.toString
|> List.iter (printfn "%s")

// Get the doserules for the test case
GenPresProduct.filter "gentamicine" "" "iv"
|> Array.filter (fun gpp -> 
    gpp.GenericProducts
    |> Array.exists (fun gp -> gp.Id = 3689)
)
|> DoseRule.GStand.map
// Pretty print
|> List.map Informedica.GenForm.Lib.DoseRule.toString
|> List.iter (printf "%s")
