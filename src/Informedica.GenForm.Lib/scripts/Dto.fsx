
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


GenPresProduct.filter true "clonidine" "drank" "oraal"
|> Seq.collect (fun gpp ->
    gpp.GenericProducts
    |> Seq.collect (fun gp ->
        gp.Route
        |> Seq.map (fun r ->
            printfn "GPK: %i" gp.Id

            {
                Dto.dto with
                    AgeInMo = 12.
                    WeightKg = 10.
                    LengthCm = 60.
                    GPK = 9999 // gp.Id
                    Generic = "clonidine"
                    Shape = "drank"
                    Route = "or"
                    IsRate = false
                    MultipleUnit = "mcg"
                    RateUnit = ""
            }
            |> Dto.processDto2
        )
    )
)

|> Seq.iter (fun dto ->
    printfn "%A" dto
    dto.Text
    |> Markdown.htmlToBrowser
)

{
    Dto.dto with
        AgeInMo = 12.
        WeightKg = 10.
        GPK = 78514
        Route = "or"
}
|> Dto.processDto2
|> (fun dto -> dto.Text |> Markdown.toBrowser)

"iv" 
|> Mapping.mapRoute Mapping.AppMap Mapping.GStandMap


Informedica.GenProduct.Lib.DoseRule.get()
|> Array.collect (fun dr ->
    dr.Routes
)
|> Array.distinct
|> Array.sort
|> Array.iter (printfn "%s")



GenPresProduct.filter true "clonidine" "drank" "oraal"
|> Seq.collect (fun gpp ->
    gpp.GenericProducts
    |> Seq.map (fun gp -> gp.Substances)
)

open Informedica.GenUtils.Lib.BCL

module GPP = GenPresProduct

let find (dto : Dto.Dto) =
    let rte = dto.Route |> Mapping.mapRoute Mapping.AppMap Mapping.GStandMap

    let gpps =
        let ps = dto.GPK |> GPP.findByGPK 

        if ps |> Array.length = 0 then
            GenPresProduct.filter true dto.Generic dto.Shape rte
        else ps
        |> Array.toList

    match gpps with
    | [gpp] -> 
        let lbl, conc, unt, tps = 
            let gp = 
                if dto.GPK = 0 then
                    if gpp.GenericProducts |> Seq.length = 1 then gpp.GenericProducts |> Seq.head |> Some
                    else None
                else gpp.GenericProducts |> Seq.tryFind (fun p -> p.Id = dto.GPK)
            match gp with
            | Some gp -> 
                let conc, unt =
                    match gp.Substances |> Seq.tryFind (fun s -> s.SubstanceName |> String.equalsCapInsens gpp.Name) with
                    | Some s -> s.SubstanceQuantity, s.SubstanceUnit
                    | None -> 0., ""

                let tps =
                    gp.PrescriptionProducts
                    |> Array.fold (fun acc pp ->
                        pp.TradeProducts
                        |> Array.map (fun tp -> tp.Label)
                        |> Array.toList
                        |> List.append acc
                    ) []
                    |> String.concat "||"
                gp.Label, conc, unt, tps
            | None -> "", 0., "", ""

        gpp.Name, gpp.Shape, lbl, conc, unt, tps
    | _ -> 
        printfn "Could not find product %s %s %s with GPK: %i" dto.Generic dto.Shape dto.Route dto.GPK
        "", "", "", 0., "", ""


{
    Dto.dto with
        AgeInMo = 12.
        WeightKg = 10.
        LengthCm = 60.
        //GPK = gp.Id
        Generic = "clonidine"
        Shape = "drank"
        Route = "or"
        IsRate = false
        MultipleUnit = "mcg"
        RateUnit = ""
}
|> find
