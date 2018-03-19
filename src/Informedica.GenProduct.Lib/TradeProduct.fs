namespace Informedica.GenProduct.Lib

module TradeProduct =

    open Informedica.GenUtils.Lib

    type TradeProduct =
        {
            Id: int
            Name : string
            Label : string
            Brand : string
            Company : string
            Denominator : int
            ConsumerProducts : ConsumerProduct.ConsumerProduct []
        }

    let create id nm lb br cm dn ps =
        {
            Id = id
            Name = nm
            Label = lb
            Brand = br
            Company = cm
            Denominator = dn
            ConsumerProducts = ps
        }

    let _get id =
        Zindex.BST031T.records ()
        |> Array.filter (fun r  -> 
            r.MUTKOD <> 1 &&
            r.PRKODE = id
        )
        |> Array.map (fun r -> 
            let nm = Names.getName r.HPNAMN Names.Full
            let lb = Names.getName r.HPNAMN Names.Label
            let ps = ConsumerProduct.get r.HPKODE
            create r.HPKODE nm lb r.MSNAAM r.FSNAAM r.HPDEEL ps
        )

    let get : int -> TradeProduct [] = Memoization.memoize _get