namespace Informedica.GenProduct.Lib

module GenericProduct =
    
    open Informedica.GenUtils.Lib
    open Informedica.GenUtils.Lib.BCL

    type GenericProduct =
        {
            Id : int
            Name : string
            Label : string
            ATC : string
            ATCName : string
            Shape : string
            Route : string []
            Substances : Substance []
            PrescriptionProducts : PrescriptionProduct.PrescriptionProduct []
        }
    and Substance = 
        {
            SubstanceName : string
            SubstanceQuantity : float
            SubstanceUnit : string
            GenericName : string
            GenericQuantity : float
            GenericUnit : string
            ShapeUnit : string
        }

    let createSubstance sn sq su gn gq gu un =
        {
            SubstanceName = sn
            SubstanceQuantity = sq
            SubstanceUnit = su
            GenericName = gn
            GenericQuantity = gq
            GenericUnit = gu
            ShapeUnit = un
        }

    let create id nm lb ac an sh rt ss ps =
        {
            Id = id
            Name = nm
            Label = lb
            ATC = ac
            ATCName = an 
            Shape = sh
            Route = rt
            Substances = ss
            PrescriptionProducts = ps
        }


    let getRoutes (p : Zindex.BST711T.BST711T) =

        let rt =
            Zindex.BST051T.records ()
            |> Array.filter (fun r -> 
                r.MUTKOD <> 1 && 
                r.GPKODE = p.GPKODE
            )
            |> Array.collect (fun r ->
                Zindex.BST031T.records ()
                |> Array.filter (fun r' ->
                    r'.MUTKOD <> 1 &&
                    r'.PRKODE = r.PRKODE 
                )    
            )
            |> Array.collect (fun r ->
                Zindex.BST760T.records ()
                |> Array.filter (fun r' ->
                    r'.MUTKOD <> 1 &&
                    r'.HPKODE = r.HPKODE
                )
                |> Array.map(fun r' -> r'.ENKTDW)
            )
            |> Array.distinct
            |> Array.map (fun tdw ->
                Names.getThes tdw Names.Route Names.Fifty
            )
            |> Array.filter String.notEmpty

        if rt |> Array.isEmpty |> not then rt
        else 
            [| Names.getThes p.GPKTWG Names.Route Names.Fifty |]

                        
    let private _get gpks = 
        Zindex.BST711T.records ()
        |> Array.filter (fun gp -> 
            gp.MUTKOD <> 1 &&
            gp.GPKTVR <> 980 && // filter shape <> "NIET VAN TOEPASSING"
            (gpks |> List.isEmpty ||
             gpks
             |> List.exists ((=) gp.GPKODE)))
        |> Array.map (fun gp -> 
            let nm = Names.getName gp.GPNMNR Names.Full
            let lb = Names.getName gp.GPNMNR Names.Label

            let an = 
                match
                    Zindex.BST801T.records ()
                    |> Array.tryFind (fun atc -> 
                        atc.MUTKOD <> 1 &&
                        atc.ATCODE = gp.ATCODE
                    ) with
                | Some atc' -> atc'.ATOMS
                | None     -> ""
            let sh = Names.getThes gp.GPKTVR Names.Shape Names.Fifty
            let rt = getRoutes gp
            let ps = PrescriptionProduct.get gp.GPKODE
            let un = Names.getThes gp.XPEHHV Names.ShapeUnit Names.Fifty
            let ss = 
                let gss =
                    Zindex.BST715T.records ()
                    |> Array.filter (fun gs ->
                        gs.GSKODE = gp.GSKODE &&
                        gs.MUTKOD <> 1 &&
                        gs.GNMWHS = "W"
                    )

                let gns = 
                    Zindex.BST750T.records ()
                    |> Array.filter (fun gn ->
                        gn.MUTKOD <> 1 &&
                        gss |> Array.exists(fun gs -> gs.GNNKPK = gn.GNGNK)
                    )

                query {
                    for gs in gss do
                    join gn in gns
                        on (gs.GNNKPK = gn.GNGNK) 
                    // Get substance name and concentration
                    let cn =
                        let hps =
                            Zindex.BST051T.records ()
                            |> Array.filter (fun pr ->
                                pr.GPKODE = gp.GPKODE
                            ) 
                            |> Array.collect (fun pr ->
                                Zindex.BST031T.records ()
                                |> Array.filter (fun hp ->
                                    hp.MUTKOD <> 1 &&
                                    hp.PRKODE = pr.PRKODE
                                )
                            )

                        let gns' =
                            Zindex.BST750T.records ()
                            |> Array.filter (fun gn' ->
                                gn.GNGNAM.Contains(gn'.GNGNAM)
                            )


                        query {
                            for ig in (Zindex.BST701T.records ()
                                       |> Array.filter (fun r -> r.GNMWHS = "W")) do
                            join gn' in gns'
                                on (ig.GNSTAM = gn'.GNGNK)
                            join hp in hps 
                                on (ig.HPKODE = hp.HPKODE)
                            
                            let u = Names.getThes ig.XNMINE Names.GenericUnit Names.Fifty
                            
                            select (gn'.GNGNAM, ig.GNMINH, u)
                        } 
                        |> Seq.toArray
                                    
                    let gu = Names.getThes gs.XNMOME Names.GenericUnit Names.Fifty

                    select 
                     (
                        if cn |> Array.isEmpty then
                            createSubstance gn.GNGNAM  gs.GNMOMH gu gn.GNGNAM gs.GNMOMH gu un
                        else 
                            let sn, sq, su = cn |> Array.head
                            createSubstance sn sq su gn.GNGNAM gs.GNMOMH gu un
                     )
                     
                }
                |> Seq.distinct
                |> Seq.toArray
                

            create gp.GPKODE nm lb gp.ATCODE an sh rt ss ps
        )

    let get : int list -> GenericProduct [] = Memoization.memoize _get


