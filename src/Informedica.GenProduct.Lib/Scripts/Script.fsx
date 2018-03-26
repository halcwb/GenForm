#load "genproduct-files.fsx"

#time

open System

// let pwd = @"/Users/hal/Development/GenFormService/src/Informedica.GenProduct.Lib/"
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ + "/../../../"

open Informedica.GenUtils.Lib
open Informedica.GenUtils.Lib.BCL
open Informedica.GenProduct.Lib

// File
File.exists <| FilePath.GStandPath + "BST000T"


// ProductRange
ProductRange.data ()
//|> Array.filter (fun d -> d.GPK |> Option.isNone)
|> Array.iter   (printfn "%A")

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


GenPresProduct.filter "paracetamol" "" "rectaal"
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
RuleFinder.createFilter (Some 12.) (Some 10.) None None "paracetamol" "" "iv"
|> RuleFinder.find
|> RuleFinder.convertToResult

// Creating gp: PARACETAMOL INFVLST 10MG/ML
// Real: 00:00:00.041, CPU: 00:00:00.036, GC gen0: 1, gen1: 0
// val it : RuleFinder.RuleResult =
//   {DoseRules =
//     [|"217494, PARACETAMOL INFVLST 10MG/ML, Groep: intensieve, Type: Standaard, Route: INTRAVENEUS, Indicatie: 4. Algemeen, Leeftijd: 1 - 216 maanden, Gewicht: tot 66.7 kg, Freq: 4 per dag, Norm/Kg: tot 1.5 ml";
//       "346721, PARACETAMOL INFVLST 10MG/ML, Groep: intensieve, Type: Standaard, Route: INTRAVENEUS, Indicatie: 4. Algemeen, Leeftijd: 1 - 216 maanden, Gewicht: tot 50 kg, Freq: 1 eenmalig, Norm/Kg: tot 2 ml"|];
//    Doses = [|{Freq = {Frequency = 4.0;
//                       Time = "per dag";};
//               NormDose = {Min = None;
//                           Max = None;};
//               AbsDose = {Min = None;
//                          Max = None;};
//               NormKg = {Min = None;
//                         Max = Some 60.0;};
//               AbsKg = {Min = None;
//                        Max = None;};
//               NormM2 = {Min = None;
//                         Max = None;};
//               AbsM2 = {Min = None;
//                        Max = None;};
//               Unit = "MILLIGRAM";}; {Freq = {Frequency = 1.0;
//                                              Time = "eenmalig";};
//                                      NormDose = {Min = None;
//                                                  Max = None;};
//                                      AbsDose = {Min = None;
//                                                 Max = None;};
//                                      NormKg = {Min = None;
//                                                Max = Some 20.0;};
//                                      AbsKg = {Min = None;
//                                               Max = None;};
//                                      NormM2 = {Min = None;
//                                                Max = None;};
//                                      AbsM2 = {Min = None;
//                                               Max = None;};
//                                      Unit = "MILLIGRAM";}|];}
