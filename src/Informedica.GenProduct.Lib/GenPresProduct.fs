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
            DisplayName: string
            Synonyms: string []
        }

    let create nm sh rt ph gps dpn sns =
        {
            Name = nm
            Shape = sh
            Route = rt
            Pharmacologic = ph
            GenericProducts = gps
            DisplayName = dpn
            Synonyms = sns
        }

    let private parse (prs : ProductRange.ProductRange []) =
        let gpks =  prs |> Array.map (fun pr -> pr.GPK |> Option.get)

        GenericProduct.get (gpks |> Array.toList)
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
            let dpn = 
                prs 
                |> Array.filter (fun pr -> 
                    if pr.GPK |> Option.isNone then false
                    else
                        gps
                        |> Array.exists (fun gp ->
                            pr.GPK |> Option.get = gp.Id
                        )
                        
                )
                |> Array.fold (fun acc pr ->
                    if acc = "" then pr.Generic
                    else acc
                ) ""
            let ph = 
                gps 
                |> Array.collect (fun gp -> 
                    Zindex.BST801T.records ()
                    |> Array.filter (fun atc -> 
                        atc.ATCODE.Trim() = gp.ATC.Substring(0, 5))
                    |> Array.map (fun atc -> atc.ATOMS))
                |> Array.distinct
            create nm sh rt ph gps dpn [||])

    let private _get (prs : ProductRange.ProductRange []) = 
        if FilePath.productCache |> File.exists then
            FilePath.productCache
            |> Json.getCache 
            |> Array.filter (fun gpp -> 
                gpp.GenericProducts
                |> Array.exists (fun gp -> 
                    prs 
                    |> Array.exists (fun pr -> pr.GPK |> Option.get = gp.Id)
                )
            )
        else
            printfn "No cache creating GenPresProduct"
            let gsps = parse prs
            gsps |> Json.cache FilePath.productCache
            gsps

    let private get = Memoization.memoize _get

    let getAssortment () = 
        ProductRange.data ()
        |> Array.filter (fun pr -> pr.GPK |> Option.isSome)
        |> get


    let toString (gpp : GenPresProduct) =
        gpp.Name + " " + gpp.Shape + " " + (gpp.Route |> String.concat "/")

    let filter n s r =
        getAssortment ()
        |> Array.filter (fun gpp ->
            gpp.Name  |> String.equalsCapInsens n && 
            (s = "" || gpp.Shape |> String.equalsCapInsens s) && 
            (r = "" || gpp.Route |> Array.exists (fun r' -> r' |> String.equalsCapInsens r))

        )

    let findByGPK gpk =
        getAssortment ()
        |> Array.filter (fun gpp ->
            gpp.GenericProducts
            |> Array.exists (fun gp -> gp.Id = gpk)
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