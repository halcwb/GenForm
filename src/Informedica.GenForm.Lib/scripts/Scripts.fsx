#load "references.fsx"
open Informedica.GenUtils.Lib

#time

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ + "/../"


open Informedica.GenProduct.Lib

type GenFormItem =
    {
        GenPresProduct : GenPresProduct.GenPresProduct
        ResultProduct : ProductRange.ProductRange
        Rules : RuleFinder.RuleResult
    }

let create gpp rp rs = 
    {
        GenPresProduct = gpp
        ResultProduct = rp
        Rules = rs
    }


let get age wght bsa rt (rp : ProductRange.ProductRange) =
    if rp.GPK |> Option.isNone then None
    else
        match [ rp.GPK |> Option.get ] |>  GenPresProduct.get with
        | [| gpp |] -> 
            printfn "%A %A" rp.GPK rp.Route
            RuleFinder.createFilter age wght bsa rp.GPK "" "" (if rt = "" then rp.Route else rt)
            |> RuleFinder.find
            |> RuleFinder.convertToResult
            |> create gpp rp 
            |> Some
        | _ -> None



ProductRange.data ()
|> Array.take 1
//|> (fun pr -> printfn "%A" pr; pr)
|> Array.map get
