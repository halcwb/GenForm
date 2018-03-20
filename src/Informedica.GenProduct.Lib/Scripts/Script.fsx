#load "references.fsx"

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


// Create a rule filter
let filter = RuleFinder.createFilter (Some 0.) (Some 1.6) None None "gentamicine" "" "iv"

// Get all products using the filter
GenPresProduct.filter filter.Generic filter.Shape filter.Route


// Get all dose rules for the filter
RuleFinder.find filter
//|> Array.filter (fun dr -> dr.CareGroup = "intensieve")
|> RuleFinder.convertToResult
|> ignore
