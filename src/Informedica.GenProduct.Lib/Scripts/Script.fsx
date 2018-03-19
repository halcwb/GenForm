#load "references.fsx"
open Informedica.GenProduct.Lib.GenericProduct
open Informedica.GenProduct.Lib.GenericProduct
open Informedica.GenProduct.Lib.DoseRule

#time

open System

// let pwd = @"/Users/hal/Development/GenFormService/src/Informedica.GenProduct.Lib/"
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ + "/../"

open Informedica.GenUtils.Lib
open Informedica.GenUtils.Lib.BCL

// Memoization
let fib x =
    let rec fib' n =
        if n < 2 then n
        else
            fib' (n - 1) + fib' (n - 2)
    fib' x

let fibMem = Memoization.memoize fib
40 |> fibMem
40 |> fibMem

// Int32
"32" |> Int32.parse

// Char
'a' |> Char.isCapital

// Double
1. / 3. |> Double.fixPrecision 2
10. * (1. / 3.) |> Double.fixPrecision 2
100. * (1. / 3.) |> Double.fixPrecision 2

// Array
[|'a'..'z'|]
|> Array.pickArray [1; 3; 5]
|> Array.toString

[|[|'a'..'z'|];[|'d'..'g'|]|]
|> Array.arrayFilter (fun c -> c = 'a' || c = 'b')


// List
['a'..'z']
|> List.pickList [1; 3; 5]
|> List.toString

[['a'..'z'];['d'..'g']] 
|> List.listFilter (fun c -> c = 'a' || c = 'b')


open Informedica.GenProduct.Lib

// File
File.exists <| File.GStandPath + "BST000T"

// Json cache

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


GenPresProduct.filter "paracetamol" "zetpil" "rectaal"
|> Array.map GenPresProduct.toString
|> Array.iter (printfn "%s")

// Dose rules
DoseRule.get ()
|> Array.length

// Get all dose rules for paracetamol
DoseRule.get()
|> Array.filter (fun dr ->
    dr.GenericProduct |> Array.exists (fun gp -> gp.Name |> String.startsWithCapsInsens "paracetamol")
)
|> Array.length

let toString (dr : DoseRule.DoseRule) =
    let addString lbl s = 
        if s = "" then ""
        else
            lbl + ": " + s + ", "

    let freqToString (fr: DoseRule.Frequency) =
        (fr.Frequency |> string) + " " + (fr.Time |> string)

    let optToString pre post o = 
        let s =
            if o |> Option.isSome then o |> Option.get |> string else "" 
        if s = "" then "" else pre + " " +  s + " " + post

    let minMaxToString u (mm: DoseRule.MinMax) = 
        let s =
            match mm.Min, mm.Max with
            | None, None -> ""
            | Some min, None -> "vanaf " + (min |> string)
            | None, Some max ->
                if max = 0. then "" else "tot " + (max |> string)
            | Some min, Some max -> (min |> string) + " - " + (max |> string)
        if s = "" then "" else s + " " + u

    if dr.GenericProduct |> Array.length = 1 then
        dr.GenericProduct.[0].Name + ": "
    else ""
    + (addString "Indicatie" dr.Indication)
    + (addString "Geslacht" dr.Gender)
    + (addString "Leeftijd" (dr.Age |> minMaxToString "maanden"))
    + (addString "Oppervlak" (dr.BSA |> minMaxToString "m2"))
    + (addString "Min. Leeftijd" (dr.MinAge |> optToString "vanaf" "maanden"))
    + (addString "Gewicht" (dr.Weight |> minMaxToString "kg"))
    + (addString "Frequentie" (dr.Freq |> freqToString))
    + (addString "Dosering" (dr.Norm |> minMaxToString dr.Unit))
    + (addString "Dose Per kg" (dr.NormKg |> minMaxToString dr.Unit))
    + (addString "Dose Per m2" (dr.NormM2 |> minMaxToString dr.Unit))
    + (addString "Grens Per kg" (dr.AbsKg |> minMaxToString dr.Unit))
    + (addString "Grens Per m2" (dr.AbsM2 |> minMaxToString dr.Unit))
    + (addString "Abs grens" (dr.Abs |> minMaxToString dr.Unit))


GenPresProduct.filter "paracetamol" "zetpil" "rectaal"
|> Array.collect (fun gpp -> 
    gpp.GenericProducts 
    |> Array.map (fun gp -> gp.Id)
)
|> Array.collect (fun gpk ->
    DoseRule.get()
    |> Array.filter (fun dr -> dr.GenericProduct |> Array.exists (fun gp -> gp.Id = gpk) 
    )
    |> Array.map toString
    |> Array.distinct
)
|> Array.iter (printfn "%s")
