#load "genproduct-files.fsx"

#time

open System

let pwd = Environment.GetEnvironmentVariable("HOME")
Environment.CurrentDirectory <- pwd + "/Development/GenFormService/" //__SOURCE_DIRECTORY__ + "/../../../"

open Informedica.GenUtils.Lib
open Informedica.GenUtils.Lib.BCL
open Informedica.GenProduct.Lib

// File
File.exists <| FilePath.GStandPath + "BST000T"


// ProductRange
ProductRange.data ()
//|> Array.filter (fun d -> d.GPK |> Option.isNone)
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
|> Seq.sortBy (fun gpp -> gpp.Name, gpp.Shape, gpp.Route)
|> Seq.map (fun gpp -> gpp.Name + ", " + gpp.Shape + ", " + (gpp.Route |> String.concat "/"))
//|> Seq.take 10
|> Seq.iter (fun n -> printfn "%s" n)


GenPresProduct.filter "paracetamol" "" ""
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
RuleFinder.createFilter None None None None "gentamicine" "" ""
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
|> Array.sortBy (fun f -> (f.Time, f.Frequency))
|> Array.iter (fun f -> printfn "%s %s" (f.Frequency |> string) (f.Time))


// Get all distinct frequencies times
DoseRule.frequencies ()
|> Array.map (fun f -> f.Time)
|> Array.distinct
|> Array.sort
|> Array.iter (printfn "%s")


DoseRule.get()
|> Array.filter (fun dr ->
    dr.Freq.Time |> String.equalsCapInsens "per dag" &&
    dr.Freq.Frequency > 24.
)
|> Array.iter (fun dr -> dr |> DoseRule.toString ", " |> (printfn "%s"))



GenPresProduct.filter "morfine" "" "iv"
|> Array.collect (fun gpp -> 
    gpp.GenericProducts
)
|> Array.map (fun gp ->
    gp.Name
)

// Get alle names and displaynames
GenPresProduct.getAssortment ()
|> Array.map (fun gpp -> gpp.Name, gpp.DisplayName)
|> Array.iter (fun (nm, dpn) ->
    printfn "%s, %s" nm dpn
)

// Get all dose rules for age 12 months weight 10 kg paracetamol rect
RuleFinder.createFilter (Some 12.) (Some 10.) None None "paracetamol" "" ""
|> RuleFinder.find
|> Array.map (fun r -> DoseRule.toString ", " r |> printfn "%s"; r)
|> RuleFinder.convertToResult


DoseRule.get ()
|> Array.filter (fun dr ->
    dr.Freq.Time = "eenmalig" && dr.Freq.Frequency > 1.0
) 
|> Array.iter (fun dr -> 
    dr 
    |> DoseRule.toString ", " 
    |> printfn "%s"
)

GenPresProduct.getAssortment ()
|> Array.iter (fun gpp ->
    let dbls = 
        GenPresProduct.getAssortment ()
        |> Array.filter (fun gpp_ -> gpp.Name = gpp_.Name && gpp.Shape = gpp_.Shape)
    if dbls |> Array.length > 1 then 
        dbls 
        |> Array.iter (fun gpp__ ->
            printfn "%s %s %s" gpp__.Name gpp__.Shape (gpp__.Route |> String.concat "/")
        )
)

GenPresProduct.getAssortment ()
|> Array.collect (fun gpp ->
    gpp.GenericProducts 
    |> Array.collect (fun gp ->
        gp.Substances 
        |> Array.map (fun s -> s.GenericUnit)
    )
) |> Array.distinct |> Array.sort

DoseRule.get ()
|> Array.filter (fun dr ->
    dr.PrescriptionProduct |> Array.length > 0 &&
    dr.TradeProduct |> Array.length = 0
) 
|> Array.map (DoseRule.toString ", ")
|> Array.iter (printfn "%s")

let rts =
    DoseRule.get()
    |> Array.collect (fun dr ->
        dr.Routes
    ) |> Array.sort |> Array.distinct

GenPresProduct.getRoutes()
|> Array.filter (fun r -> 
    rts
    |> Array.exists ((=) r)
    |> not
)

