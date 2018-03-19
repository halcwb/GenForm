namespace Informedica.GenProduct.Lib

module Substance =

    open Informedica.GenUtils.Lib

    type Substance =
        {
            Id : int
            Name : string
            Mole : float
            MoleReal : float
        }

    let create id nm ms mr = 
        {
            Id = id
            Name = nm
            Mole = ms
            MoleReal = mr
        }

    let cache (sbs : Substance []) = Json.cache File.substanceCache sbs

    let parse () =
        Zindex.BST750T.records ()
        |> Array.filter (fun r -> r.MUTKOD <> 1)
        |> Array.map (fun r ->
            create r.GNGNK r.GNGNAM r.GNMOLE r.GNMOLS)
        
    let _get _ =
        if File.substanceCache  |> File.exists then
            File.substanceCache
            |> Json.getCache
        else
            printfn "No cache creating Substance"
            let substs = parse ()
            substs |> Json.cache File.substanceCache
            substs

    let get : unit -> Substance [] =
        Memoization.memoize _get
