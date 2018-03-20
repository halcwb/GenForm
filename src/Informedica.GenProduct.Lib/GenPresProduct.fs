namespace Informedica.GenProduct.Lib

module GenPresProduct =
    
    open Informedica.GenUtils.Lib.BCL
    open Informedica.GenUtils.Lib

    type GenPresProduct =
        {
            Name : string
            Shape : string
            Route : string []
            Pharmacologic : string []
            GenericProducts : GenericProduct.GenericProduct []
        }

    let create nm sh rt ph gps =
        {
            Name = nm
            Shape = sh
            Route = rt
            Pharmacologic = ph
            GenericProducts = gps
        }

    let parse gpks =
        GenericProduct.get gpks
        |> Array.map (fun gp -> 
            let n = 
                gp.Substances 
                |> Array.map (fun s -> s.SubstanceName)
                |> Array.distinct
                |> Array.fold (fun a s -> 
                    if a = "" then s 
                    else a + "/" + s) ""
            printfn "Creating gpp: %s" n
            ((n, gp.Shape, gp.Route), gp))
        |> Array.groupBy (fun (key, gp) -> key)
        |> Array.map (fun ((nm, sh, rt), xs) -> 
            let gps = xs |> Array.map (fun (_, gp) -> gp)
            let ph = 
                gps 
                |> Array.collect (fun gp -> 
                    Zindex.BST801T.records ()
                    |> Array.filter (fun atc -> 
                        atc.ATCODE.Trim() = gp.ATC.Substring(0, 5))
                    |> Array.map (fun atc -> atc.ATOMS))
                |> Array.distinct
            create nm sh rt ph gps)

    let _get gpks = 
        if File.productCache |> File.exists then
            File.productCache
            |> Json.getCache 
        else 
            printfn "No cache creating GenPresProduct"
            let gsps = parse gpks
            gsps |> Json.cache File.productCache
            gsps

    let get : int list -> GenPresProduct [] = Memoization.memoize _get

    let getAssortment () = 
        ProductRange.data ()
        |> Seq.map (fun d -> d.GPK)
        |> Seq.toList
        |> List.filter Option.isSome
        |> List.map Option.get
        |> get


    let toString (gpp : GenPresProduct) =
        gpp.Name + " " + gpp.Shape + " " + (gpp.Route |> String.concat "/")

    let filter n s r =
        getAssortment()
        |> Array.filter (fun gpp ->
            gpp.Name  |> String.equalsCapInsens n && 
            (s = "" || gpp.Shape |> String.equalsCapInsens s) && 
            (r = "" || gpp.Route |> Array.exists (fun r' -> r' |> String.equalsCapInsens r))

        )
        

(*
    let getPediatric () =
        getAssortment () 
        |> Array.filter (fun gpp -> 
            FormularyParser.WebSiteParser.getFormulary ()
            |> Array.exists (fun d -> 
                gpp.GenericProducts
                |> Array.exists (fun gp ->
                    let gpAtc, dAtc = gp.ATC.Trim(), d.Atc.Trim()
                    let gpn, dn = 
                        gpp.Name |> String.toLower |> String.trim, 
                        d.Generic |> String.toLower |> String.trim
                    gpAtc = dAtc ||
                    (gpAtc |> String.startsWith dAtc &&
                     (dn |> String.contains gpn || (gpn |> String.contains dn)))
                )
            )
        ) 
*)