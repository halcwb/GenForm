﻿namespace Informedica.GenProduct.Lib

module PrescriptionProduct =

    open Informedica.GenUtils.Lib

    type PrescriptionProduct =
        {
            Id : int
            Name : string
            Label  : string
            Quantity : float
            Unit : string
            Container : string
            TradeProducts : TradeProduct.TradeProduct []
        }

    let create id nm lb qt un ct ps =
        {
            Id = id
            Name = nm
            Label = lb
            Quantity = qt
            Unit = un
            Container = ct
            TradeProducts = ps
        }

    let _get id = 
        Zindex.BST051T.records ()
        |> Array.filter (fun r -> 
            r.MUTKOD <> 1 && 
            r.GPKODE = id && 
            Zindex.BST050T.records ()
            |> Array.exists (fun r' -> 
                r'.PRKODE = r.PRKODE
            )
        )
        |> Array.map (fun r -> 
            let p = 
                Zindex.BST050T.records ()
                |> Array.find (fun r' -> r'.PRKODE = r.PRKODE)
            let nm = Names.getName p.PRNMNR Names.Full
            let lb = Names.getName p.PRNMNR Names.Label
            let un = Names.getThes r.XPEHHV 1 Names.Fifty
            let ct = Names.getThes r.HPEMBT 73 Names.Fifty
            let ps = TradeProduct.get r.PRKODE
            create r.PRKODE nm lb r.HPGALG un ct ps
        )

    let get : int -> PrescriptionProduct [] = Memoization.memoize _get
