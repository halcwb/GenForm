
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
                    //GPK = gp.Id
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


