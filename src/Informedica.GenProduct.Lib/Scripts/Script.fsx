#load "references.fsx"

#time

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Linq
open System.Collections
open System.Collections.Generic

open Microsoft.FSharp
open Microsoft.FSharp.Core
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Linq.RuntimeHelpers

let pwd = @"/Users/hal/Development/GenFormService/src/Informedica.GenProduct.Lib/"
Environment.CurrentDirectory <- pwd //__SOURCE_DIRECTORY__

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
type Test = { name: string }

{ name = "Test"}
|> Json.cache File.groupCache

// Substance
Substance.get()
|> Array.sortBy (fun s -> s.Name)
|> Array.iter (fun s -> printfn "%s" s.Name)
|> ignore

// Consumer product
ConsumerProduct.get(100)
|> printf "%A"


