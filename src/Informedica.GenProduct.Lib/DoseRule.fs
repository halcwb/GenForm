namespace Informedica.GenProduct.Lib

module DoseRule =

    open System
    open System.Text
    open Informedica.GenUtils.Lib.BCL
    open Informedica.GenUtils.Lib

    type DoseRule =
        {
            Id : int
            CareGroup : string
            Usage : string
            DoseType : string
            GenericProduct : GenericProduct[]
            PrescriptionProduct : Product[] 
            TradeProduct : Product[]
            Route : string
            Indication : string
            MinAge : int Option
            HighRisk : bool
            Gender : string
            Age : MinMax 
            Weight : MinMax 
            BSA : MinMax 
            Freq : Frequency 
            Norm : MinMax 
            Abs : MinMax 
            NormKg : MinMax 
            AbsKg : MinMax 
            NormM2 : MinMax 
            AbsM2 : MinMax 
            Unit : string
        }
    and Product = { Id: int; Name: string }
    and GenericProduct = { Id: int; Name: string; Route: string; Unit: string }
    and Frequency = { Frequency: float; Time: string }
    and MinMax = { Min: float Option; Max: float Option }

    let toString del (r: DoseRule)  =
        let minMaxToString n u p (mm: MinMax) s =
            let mms =
                match mm.Min, mm.Max with
                | Some min, Some max -> 
                    let min = Double.fixPrecision p min |> string
                    let max = Double.fixPrecision p max |> string
                    sprintf "%s - %s" min max
                | Some min, None -> 
                    let min = Double.fixPrecision p min |> string
                    sprintf "vanaf %s" min
                | None, Some max -> 
                    if max = 0. then ""
                    else 
                        let max = Double.fixPrecision p max |> string
                        sprintf "tot %s" max
                | None, None -> ""  
            if mms = "" then s
            else
                s + n + ": " + mms + " " + u + del

        let adds s1 s2 s3 = 
            if s2 |> String.IsNullOrWhiteSpace then s3
            else 
                let s3 = if s1 = "" then s3 else s3 + s1 + ": "
                s3 + s2 + del

        let gp = 
            r.GenericProduct |> Seq.fold(fun a gp -> 
                let s' = if a |> String.IsNullOrWhiteSpace then "" else ", "
                s' + gp.Name) ""

        let pp = 
            r.PrescriptionProduct |> Seq.fold(fun a gp -> 
                let s' = if a |> String.IsNullOrWhiteSpace then "" else ", "
                s' + gp.Name) ""

        let tp = 
            r.TradeProduct |> Seq.fold(fun a gp -> 
                let s' = if a |> String.IsNullOrWhiteSpace then "" else ", "
                s' + gp.Name) ""

        let s = "" + (string r.Id) + del
        let s = s |> adds "" gp 
        let s = s |> adds "" pp 
        let s = s |> adds "" tp
        let s = s |> adds "Gebruik" r.Usage
        let s = s |> adds "Groep" r.CareGroup
        let s = s |> adds "Type" r.DoseType
        let s = s |> adds "Route" r.Route 
        let s = s |> adds "Indicatie" r.Indication 

        let s = if r.HighRisk then s + "Hig Risk " else s

        let s = s |> adds "" r.Gender

        let s = 
            match r.MinAge with
            | Some x -> s + "Min. Leeftijd: " + (string x) + " maanden" + del
            | None   -> s

        let s = s |> minMaxToString "Leeftijd" "maanden" 1 r.Age
        let s = s |> minMaxToString "Gewicht" "kg" 2 r.Weight
        let s = s |> minMaxToString "BSA" "m2" 2 r.BSA

        let s = 
            if r.Freq.Frequency <= 0. then s
            else
                s + "Freq: " + (string r.Freq.Frequency) + " " + r.Freq.Time + del

        let s = s |> minMaxToString "Norm" r.Unit 2 r.Norm
        let s = s |> minMaxToString "Norm/Kg" r.Unit 2 r.NormKg
        let s = s |> minMaxToString "Abs" r.Unit 2 r.Abs
        let s = s |> minMaxToString "Abs/Kg" r.Unit 2 r.AbsKg

        let s = s |> minMaxToString "Norm/m2" r.Unit 2 r.NormM2
        let s = s |> minMaxToString "Abs/m2" r.Unit 2 r.AbsM2
        
        let s = s |> String.subString 0 ((s |> String.length) - (del |> String.length))
        s

    let minmax = { Min = None; Max = None }

    let createProduct id nm : Product = { Id = id; Name = nm }
    let createGenericProduct id nm rt un = { Id = id; Name = nm; Route = rt; Unit = un }
    let createFrequency fr tm = { Frequency = fr; Time = tm }
    let createMinMax mn mx = { Min = mn; Max = mx }

    let create id gr us dt gp pr tr rt ic ma hr sx ag wt bs fr no ab nk ak nm am un =
        {
            Id = id
            CareGroup = gr
            Usage = us
            DoseType = dt
            GenericProduct = gp
            PrescriptionProduct = pr
            TradeProduct = tr
            Route = rt
            Indication = ic
            MinAge = ma
            HighRisk = hr
            Gender = sx
            Age = ag
            Weight = wt
            BSA = bs
            Freq = fr
            Norm = no
            Abs = ab
            NormKg = nk
            AbsKg = ak
            NormM2 = nm
            AbsM2 = am
            Unit = un
        }

      
    let empty = 
        {
            Id = 0
            CareGroup = ""
            Usage = ""
            DoseType = ""
            GenericProduct = Array.empty
            PrescriptionProduct = Array.empty
            TradeProduct = Array.empty
            Route = ""
            Indication = ""
            MinAge = None
            HighRisk = false
            Gender = ""
            Age = minmax
            Weight = minmax
            BSA = minmax
            Freq = createFrequency 0. ""
            Norm = minmax
            Abs = minmax
            NormKg = minmax
            AbsKg = minmax
            NormM2 = minmax
            AbsM2 = minmax
            Unit = ""
        }


    let _getGenericProducts () =
        query {
            for p in Zindex.BST711T.records () do
            join hpknmr in Zindex.BST020T.records ()  
                on (p.GPNMNR = hpknmr.NMNR)
            join twg in Zindex.BST902T.records () 
                on (p.GPKTWG = twg.TSITNR)
            join un in Zindex.BST902T.records ()
                on (p.XPEHHV = un.TSITNR)
            where (twg.TSNR = 7 && un.TSNR = 1)

            where (p.MUTKOD <> 1)
            
            select 
                (
                    createGenericProduct p.GPKODE 
                                         (hpknmr.NMNAAM.Trim()) 
                                         (twg.THNM25.Trim()) 
                                         (un.THNM25.Trim())
                )
        } |> Seq.toArray


    let getGenericProducts : unit -> GenericProduct[] = 
        Memoization.memoize _getGenericProducts


    let _getPrescriptionProducts _ =
        query {
            for p in Zindex.BST050T.records () do
            join nm in Zindex.BST020T.records ()
                on (p.PRNMNR = nm.NMNR)
            where (p.MUTKOD <> 1)
            select 
                (
                    createProduct p.PRKODE
                                  (nm.NMNAAM.Trim())
                )
        } |> Seq.toArray


    let getPresciptionProducts : unit -> Product[] =
        Memoization.memoize _getPrescriptionProducts


    let _getTradeProducts _ =
        query {
            for p in Zindex.BST031T.records () do
            join nm in Zindex.BST020T.records ()
                on (p.HPNAMN = nm.NMNR)
            where (p.MUTKOD <> 1)
            select 
                (
                    createProduct p.HPKODE
                                  (nm.NMNAAM.Trim())
                )
        } |> Seq.toArray


    let getTradeProducts : unit -> Product[] =
        Memoization.memoize _getTradeProducts


    let parse () =

        let noRoute = "TOEDIENINGSWEG NIET INGEVULD"

        let gpks = getGenericProducts ()

        let minMax min max = 
            let min = if min = 0. then None else Some min
            let max = if max >= 999. then None else Some max
            createMinMax min max

        let getMinAge = 
            let tts =
                Zindex.BST920T.records ()
                |> Array.filter (fun tt -> tt.TXMOD = 17)
                |> Array.filter (fun tt -> tt.TXSRTT = 12)
                |> Array.filter (fun tt -> tt.MUTKOD <> 1)

            Zindex.BST711T.records ()
            |> Array.filter (fun x -> x.GPMLCI > 0)
            |> Array.map (fun x -> 
                let at = 
                    tts
                    |> Array.filter(fun tt -> tt.TXKODE = x.GPMLCT)
                    |> Array.map(fun tt -> tt.TXRGL.Trim())
                    |> Array.fold (fun (b: StringBuilder) s -> b.Append(s)) (new StringBuilder())
                (x.GPKODE, x.GPMLCI, at.ToString()))

        query {
            for dos in  Zindex.BST644T.records () do
            join cat in Zindex.BST643T.records ()
                on (dos.GPDDNR = cat.GPDDNR)
            join icp in Zindex.BST642T.records () 
                on (cat.GPDCAT = icp.GPDCAT)
            join bas in Zindex.BST641T.records () 
                on (icp.GPDBAS = bas.GPDBAS)
            join vas in Zindex.BST640T.records ()
                on (bas.GPKODE = vas.GPKODE)

            let funit = 
                Zindex.BST360T.records ()
                |> Array.tryFind (fun tx -> 
                    tx.MUTKOD <> 1 &&
                    tx.TTEHNR = cat.GPDFEE 
                )
                |> (fun r -> 
                    if r |> Option.isNone then ""
                    else r.Value.TTEHOM.Trim())

            let route =
                Zindex.BST902T.records ()
                |> Array.tryFind (fun tx ->
                    tx.MUTKOD <> 1 &&
                    tx.TSNR = 7 &&
                    tx.TSITNR = icp.GPKTWG
                )
                |> (fun r -> 
                    if r |> Option.isNone then ""
                    else r.Value.THNM50.Trim())

            let doseType =
                Zindex.BST902T.records ()
                |> Array.tryFind (fun tx ->
                    tx.MUTKOD <> 1 &&
                    tx.TSNR = bas.GPDCTH &&
                    tx.TSITNR = bas.GPDCOD
                )
                |> (fun r -> 
                    if r |> Option.isNone then ""
                    else r.Value.THNM50.Trim())

            let icpc =
                Zindex.BST380T.records ()
                |> Array.tryFind (fun i ->
                    i.ICPCNR1 = icp.ICPCNR1
                ) 
                |> (fun r ->
                    if r.IsNone then ""
                    else r.Value.ICPCTXT.Trim()
                )

            where (dos.MUTKOD <> 1 &&
                   cat.MUTKOD <> 1 &&
                   icp.MUTKOD <> 1 &&
                   bas.MUTKOD <> 1 &&
                   vas.MUTKOD <> 1)

            select
                ((bas.GPKODE, bas.PRKODE, bas.HPKODE), 
                { 
                    empty with 
                        Id = dos.GPDDNR
                        Route = route
                        DoseType = doseType
                        Indication = icpc
                        HighRisk = vas.GPRISC = "*"
                        Gender =
                            if vas.GPDGST = 1 then "M"
                            elif vas.GPDGST = 2 then "F" 
                            else ""
                        CareGroup =
                            if icp.GPDZCO = 1 then "niet-intensive" 
                            elif icp.GPDZCO = 2 then "intensieve" 
                            else "alle"
                        Usage =
                            if icp.ICPCTO = 0 then ""
                            elif icp.ICPCTO = 1 then "Profylactisch"
                            else "Therapeutisch"
                        Age = minMax cat.GPDLFM cat.GPDLFX
                        Weight = minMax cat.GPDKGM cat.GPDKGX
                        BSA = minMax cat.GPDM2M cat.GPDM2X
                        Freq = 
                            let u = 
                                funit 
                                |> String.replace "om de dag" "per 2 dagen"
                                |> String.replace "per half uur" "per 30 minuten"
                            createFrequency cat.GPDFAA u
                        Norm = minMax dos.GPDCNM dos.GPDCNX
                        Abs = minMax dos.GPDCAM dos.GPDCAX 
                        NormKg = minMax dos.GPDKNM dos.GPDKNX
                        AbsKg = minMax dos.GPDKAM dos.GPDKAX
                        NormM2 = minMax dos.GPDMNM dos.GPDMNX
                        AbsM2 = minMax dos.GPDMAM dos.GPDMAX
                })      
        }
        |> Seq.toArray
        // Get minage
        |> Array.map (fun (bas, r) -> 
            let r =
                {
                    r with
                        MinAge = 
                            getMinAge 
                            |> Array.tryFind (fun (gpk, _, _) -> gpk = gpk)
                            |> (fun a -> match a with | Some (_, a, _) -> a |> Some | None -> None)
                }
            (bas, r))
        // Get generic products
        |> Array.map (fun (bas, r) ->
            let (gpk, _, _) = bas
            let gpks = 
                getGenericProducts ()
                |> Array.filter (fun gp -> 
                        gp.Id = gpk
                    )
            let rt = 
                if (r.Route = "" || r.Route = noRoute) && gpks |> Array.length > 0
                then (gpks |> Array.head).Route 
                else r.Route
            let un = 
                match gpks |> Array.tryHead with
                | Some gp -> gp.Unit
                | None -> ""
            let r = 
                {
                    r with
                        GenericProduct = gpks
                        Route = rt
                        Unit = un
                }
            (bas, r))
        // Get prescription products
        |> Array.map (fun (bas, r) ->
            let (_, prk, _) = bas
            let prks = 
                getPresciptionProducts ()
                |> Array.filter (fun pp ->
                    pp.Id = prk
                )
            (bas, { r with PrescriptionProduct = prks }))
        // Get trade products
        |> Array.map (fun (bas, r) ->
            let (_, _, hpk) = bas
            let hpks = 
                getTradeProducts ()
                |> Array.filter (fun tp ->
                    tp.Id = hpk
                )
            { r with TradeProduct = hpks }
        )


    let _get () =
        if File.ruleCache |> File.exists then
            File.ruleCache
            |> Json.getCache
        else 
            printfn "No cache creating DoseRule"
            let rules = parse ()
            rules |> Json.cache File.ruleCache 
            rules

    let get : unit -> DoseRule [] = Memoization.memoize _get

    