namespace Informedica.GenProduct.Lib

module Names =

    type Name = | Full | Short | Memo | Label

    type Length = | TwentyFive | Fifty

    /// Look in BST020T Namen bestand
    let getName id nm =
        match 
            Zindex.BST020T.records ()
            |> Array.tryFind (fun r ->
                r.MUTKOD <> 1 &&
                r.NMNR = id
            ) with
        | Some r -> 
            match nm with
            | Full  -> r.NMNAAM
            | Short  -> r.NMNM40
            | Memo  -> r.NMMEMO
            | Label -> r.NMETIK
        | None -> ""
    
    /// Look in BST902T Therauri totaal
    let getThes id nr ln = 
        match
            Zindex.BST902T.records ()
            |> Array.tryFind (fun r -> 
                r.MUTKOD <> 1 &&
                r.TSITNR = id &&
                r.TSNR = nr 
            ) with
        | Some r -> match ln with | TwentyFive -> r.THNM25 | Fifty -> r.THNM50
        | None -> ""
