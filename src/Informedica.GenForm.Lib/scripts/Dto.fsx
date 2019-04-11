
#I __SOURCE_DIRECTORY__

#load "./../../../.paket/load/netstandard2.0/main.group.fsx"

#r "./../../Informedica.GenUtils.Lib/bin/Debug/netstandard2.0/Informedica.GenUtils.Lib.dll"
#r "./../../Informedica.GenUnits.Lib/bin/Debug/netstandard2.0/Informedica.GenUnits.Lib.dll"
#r "./../../Informedica.GenProduct.Lib/bin/Debug/netstandard2.0/Informedica.GenProduct.Lib.dll"

#r "netstandard"


#time


open System

let pwd = Environment.GetEnvironmentVariable("HOME")
Environment.CurrentDirectory <- 
    if pwd |> String.IsNullOrWhiteSpace then 
        __SOURCE_DIRECTORY__ + "/../../../"

    else 
        pwd + "/Development/GenForm/" //__SOURCE_DIRECTORY__ + "/../../../"


#load "./../Utils/String.fs" 
#load "./../Utils/List.fs" 
#load "./../Utils/MarkDown.fs" 
#load "./../Mapping.fs" 
#load "./../ValueUnit.fs" 
#load "./../MinMax.fs" 
#load "./../Route.fs" 
#load "./../Product.fs" 
#load "./../Patient.fs" 
#load "./../DoseRule.fs" 
#load "./../GStand.fs" 
#load "./../Dto.fs"



open Informedica.GenProduct.Lib
open Informedica.GenForm.Lib



// List all products that have no app route
GenPresProduct.get true
|> Seq.collect (fun gpp ->
    gpp.GenericProducts
    |> Seq.collect (fun gp -> 
        gp.Route
        |> Seq.map (fun r -> 
            gpp.Name, gp.Id, r |> Mapping.mapRoute Mapping.GStandMap Mapping.AppMap, r
        )
    )
)
|> Seq.filter (fun (_, _, rt, _) -> rt = "")
|> Seq.map (fun (n, _,_, r) -> n, r)
|> Seq.distinct
|> Seq.sort
|> Seq.iter (printfn "%A")


// Process all possible dtos
GenPresProduct.get true
|> Seq.collect (fun gpp ->
    gpp.GenericProducts
    |> Seq.collect (fun gp -> 
        gp.Route
        |> Seq.map (fun r -> 
            gpp.Name, gp.Id, r |> Mapping.mapRoute Mapping.GStandMap Mapping.AppMap
        )
    )
)
|> Seq.sortBy (fun (gen, _, _) -> gen)
|> Seq.map (fun (gen, id, rt) ->
    printfn "processing %i" id
    {   Dto.dto with
            AgeInMo = 300.
            WeightKg = 80.
            LengthCm = 180.
            GPK = id
            Route = rt
            IsRate = false
    } |> Dto.processDto, gen
)
|> Seq.filter (fun (dto, _) ->
    dto.Rules |> Seq.isEmpty
)
|> Seq.map snd
|> Seq.distinct
|> Seq.toList
|> (fun xs -> xs |> List.length |> printfn "count: %i"; xs)
|> List.iter (printfn "%s")


// BigRational parse failure for: 117714
GenPresProduct.get true
|> Seq.collect (fun gpp ->
    gpp.GenericProducts
    |> Seq.collect (fun gp -> 
        gp.Route
        |> Seq.map (fun r -> 
            gpp.Name, gp.Id, r |> Mapping.mapRoute Mapping.GStandMap Mapping.AppMap
        )
    )
)
|> Seq.filter (fun (_, id, _) -> id = 117714)
|> Seq.map (fun (gen, id, rt) ->
    printfn "%s" gen
    {   Dto.dto with
            AgeInMo = 300.
            WeightKg = 80.
            LengthCm = 180.
            GPK = id
            Route = rt
            IsRate = false
    } |> Dto.processDto, gen
)
|> Seq.toList


