namespace Informedica.GenForm.Lib



module Mapping =

    open System

    open Informedica.GenUtils.Lib
    open Informedica.GenUtils.Lib.BCL
    open Informedica.GenProduct.Lib


    [<Literal>]
    let unitPath = "/formulary/UnitMapping.csv"


    [<Literal>]
    let freqPath = "/formulary/FrequencyMapping.csv"


    [<Literal>]
    let routePath = "/formulary/RouteMapping.csv"


    type Mapping = AppMap | GStandMap | PedFormMap  | ValueUnitMap


    let map path m1 m2 s =
        let i1, i2 =
            match m1, m2 with
            | AppMap,    GStandMap  -> 0, 1
            | GStandMap,  AppMap    -> 1, 0
            | AppMap,    ValueUnitMap -> 0, 3
            | GStandMap,  ValueUnitMap -> 1, 3
            | ValueUnitMap, AppMap    -> 3, 0
            | ValueUnitMap, GStandMap  -> 1, 3
            | _ -> 0, 0

        let map =
            File.readAllLines path
            |> Array.skip 1
            |> Array.map (String.splitAt ';')
        if i1 = 0 && i2 = 0 || (i1 > map.Length || i2 > map.Length) then ""
        else
            map
            |> Array.filter (fun x -> x.[i1] |> String.equalsCapInsens s)
            |> Array.fold (fun acc xs ->  
                if acc = "" then xs.[i2]
                else acc + "||" + xs.[i2]
            ) ""


    let mapUnit = map (Environment.CurrentDirectory + "/" + FilePath.data + unitPath)
    

    let mapUnitFromFormularyToGenForm = mapUnit AppMap ValueUnitMap


    let mapUnitFromGStandToGenForm = mapUnit GStandMap ValueUnitMap


    let mapFreq = map (Environment.CurrentDirectory + "/" + FilePath.data + freqPath)


    let mapRoute = map (Environment.CurrentDirectory + "/" + FilePath.data + routePath)

