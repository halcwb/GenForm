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


// File
File.exists <|  "BST000T"