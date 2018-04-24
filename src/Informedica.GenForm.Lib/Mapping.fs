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

    type Mapping = FormMap | GStandMap | PedMap  | GenFormMap

    let map path m1 m2 s =
        let i1, i2 =
            match m1, m2 with
            | FormMap,    GStandMap  -> 0, 1
            | GStandMap,  FormMap    -> 1, 0
            | FormMap,    GenFormMap -> 0, 3
            | GStandMap,  GenFormMap -> 1, 3
            | GenFormMap, FormMap    -> 3, 0
            | GenFormMap, GStandMap  -> 1, 3
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

    let mapFreq = map (Environment.CurrentDirectory + "/" + FilePath.data + freqPath)

