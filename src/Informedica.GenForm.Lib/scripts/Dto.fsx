
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
                    GPK = 54550 // gp.Id
                    Generic = ""
                    Shape = ""
                    Route = "or"
                    IsRate = true
                    MultipleUnit = "mg"
                    RateUnit = ""
            }
            |> Dto.processDto
        )
    )
)

|> Seq.iter (fun dto ->
    printfn "%A" dto
    if dto.Rules |> Seq.length <> 0 then 
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
|> Dto.processDto
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
        GPK = 5533
        Generic = ""
        Shape = ""
        Route = "iv"
        IsRate = false
        MultipleUnit = "mcg"
        RateUnit = ""
}
|> find


GenPresProduct.get false
|> Seq.sortBy (fun gpp -> gpp.Name)
|> Seq.take 1000
|> Seq.collect (fun gpp ->
    
    gpp.Route
    |> Seq.map (fun r ->

        {
            Dto.dto with
                AgeInMo = 12.
                WeightKg = 10.
                LengthCm = 80.
                Generic = gpp.Name
                Shape = gpp.Shape
                Route = r
                IsRate = true
        }
        |> Dto.processDto

    )
)
|> Seq.fold (fun acc dto ->
    printfn "%A" dto
    if dto.Rules |> Seq.length <> 0 then 
        acc + "<p></p>" + dto.Text
    else acc
) ""
|> Markdown.htmlToBrowser


GenPresProduct.get false
//|> Seq.take 1
|> Seq.sortBy (fun gpp -> gpp.Name)
//|> Seq.filter (fun gpp ->
//    gpp.GenericProducts
//    |> Seq.exists(fun gp -> gp.Id = 170925)
//)
|> Seq.collect (fun gpp ->
    
    gpp.GenericProducts
    |> Seq.collect (fun gp ->
        gp.Route 
        |> Seq.map (fun r ->
            let r_ = 
                r
                |> Mapping.mapRoute Mapping.GStandMap Mapping.AppMap
                |> String.split "||"
                |> List.head

            // printfn "using route: %s for %s" r_ r
            {
                Dto.dto with
                    AgeInMo = 120.
                    WeightKg = 40.
                    LengthCm = 120.
                    GPK = gp.Id
                    Route = r_
            }
            |> Dto.processDto

        )

    )
) //|> Seq.iter (printfn "%A")
|> Seq.fold (fun acc dto ->
    if dto.Rules |> Seq.length <> 0 then 
        acc + "<p></p>" + dto.Text
    else acc
) ""
|> Markdown.htmlToBrowser



GenPresProduct.get false
|> Seq.length

GenPresProduct.get true
|> Seq.collect (fun gpp -> gpp.Route)
|> Seq.distinct
|> Seq.sort
|> Seq.iter (printfn "%s")


GenPresProduct.get true
|> Seq.collect (fun gpp ->
    gpp.GenericProducts
    |> Seq.collect (fun gp -> gp.Route)
)
|> Seq.distinct
|> Seq.sort
|> Seq.iter (printfn "%s")


{
    Dto.dto with
        AgeInMo = 12.
        WeightKg = 10.
        LengthCm = 80.
        GPK = 3387
        Route = "iv"
}
|> Dto.processDto
|> ignore

