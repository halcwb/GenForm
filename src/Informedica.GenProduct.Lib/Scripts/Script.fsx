#load "references.fsx"

#time

open System

// let pwd = @"/Users/hal/Development/GenFormService/src/Informedica.GenProduct.Lib/"
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ + "/../"

open Informedica.GenUtils.Lib
open Informedica.GenUtils.Lib.BCL
open Informedica.GenProduct.Lib

// File
File.exists <| File.GStandPath + "BST000T"

// ProductRange
ProductRange.data ()
|> Array.filter (fun d -> d.GPK |> Option.isNone)
|> Array.iter (printfn "%A")

// Substance
Substance.get()
|> Array.sortBy (fun s -> s.Name)
|> Array.iter (fun s -> printfn "%s" s.Name)
|> ignore

// Consumer product
ConsumerProduct.get(100)
|> printf "%A"

// GenPres product
GenPresProduct.getAssortment ()
|> Seq.sortBy (fun gpp -> gpp.Name)
|> Seq.map (fun gpp -> gpp.Name)
|> Seq.iter (fun n -> printfn "%s" n)


GenPresProduct.filter "gentamicine" "injectievloeistof" ""
|> Array.map GenPresProduct.toString
|> Array.iter (printfn "%s")

// Dose rules
DoseRule.get ()
|> Array.length

// Get all possible dose units
DoseRule.get()
|> Array.map (fun dr -> dr.Unit)
|> Array.distinct
|> Array.iter (printfn "%s")


// Get all dose rules for the filter
RuleFinder.createFilter (Some 0.) (Some 1.6) None None "gentamicine" "" "iv"
|> RuleFinder.find
|> RuleFinder.convertToResult
|> ignore

DoseRule.get()
|> Array.map (fun dr -> dr.Indication)
|> Array.distinct
|> Array.sort
|> Array.iter (printfn "%s")
//|> Array.length

// Get all distinct indciations
DoseRule.indications ()
|> Array.iter (printfn "%s")

// Get all distinct routes
DoseRule.routes ()
|> Array.iter (printfn "%s")

// Get all distinct frequencies
DoseRule.frequencies ()
|> Array.iter (fun f -> printfn "%s %s" (f.Frequency |> string) (f.Time))


DoseRule.get()
|> Array.filter (fun dr ->
    dr.Freq.Time |> String.equalsCapInsens "per dag" &&
    dr.Freq.Frequency > 24.
)
|> Array.iter (fun dr -> dr |> DoseRule.toString ", " |> (printfn "%s"))
