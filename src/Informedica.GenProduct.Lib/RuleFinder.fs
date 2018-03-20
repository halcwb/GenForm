namespace Informedica.GenProduct.Lib

module RuleFinder =

    open Informedica.GenUtils.Lib.BCL

    // ARTI/LESION
    // * AURICULAIR
    // * CUTAAN
    // DENTAAL
    // ENDOCERVIC
    // * ENDOTR.PULM
    // * ENDOTRACHEOPULMONAIR
    // * EPIDURAAL
    // EPIDURAAL, INTRATHECAAL, PERINEURAAL
    // EXTRACORPORAAL
    // * GASTR-ENTER
    // * IM
    // * INHALATIE
    // INTRA-ART.
    // INTRA-ARTERIEEL
    // INTRA-ARTICULAIR
    // INTRA-OCUL.
    // INTRA-UTERIEN
    // INTRABURSAAL
    // INTRACARDIAAL
    // INTRACAVERNEUS
    // INTRACORONAIR
    // * INTRADERMAAL
    // INTRALAESIONAAL
    // INTRALYMFATISCH
    // * INTRAMUSCULAIR
    // INTRAMUSCULAIR, INTRAVENEUS
    // INTRAMUSCULAIR, SUBCUTAAN
    // INTRAOSSAAL
    // INTRAPERITONEAAL
    // INTRAPLEURAAL
    // INTRATHECAAL
    // * INTRAVENEUS
    // INTRAVENEUS, SUBCUTAAN
    // * INTRAVESIC.
    // * INTRAVESICAAL
    // INTRAVITR.
    // INTRAVITREAAL
    // * IV
    // * LOKAAL
    // * NASAAL
    // * NEUS
    // NIET GESPEC
    // NVT
    // OOG
    // * OOR
    // * ORAAL
    // ORAAL/RECT
    // * OROMUCOSAAL
    // PAR./ORAAL
    // PAR/UTERIEN
    // PAR/VESICAL
    // PARABULBAIR
    // PARENT/RECT
    // PARENTERAAL
    // PERI-ARTICULAIR
    // PERIBULBAIR
    // PERINEURAAL
    // PERITONEAAL
    // * RECTAAL
    // RETROBULBAIR
    // SUBCONJUNCTIVAAL
    // * SUBCUTAAN
    // SUBLINGUAAL
    // * TRANSDERMAAL
    // TRANSDERML
    // URETHRAAL
    // UTERIEN
    // VAGINAAL

    type Route = 
        | AUR // AURICULAIR OOR
        | CUT // CUTAAN TRANSDERMAAL TRANSDERML LOKAAL
        | ENDOTR // ENDOTR.PULM ENDOTRACHEOPULMONAIR
        | EPIDUR // EPIDURAAL
        | IM // INTRAMUSCULAIR IM
        | INH // INHALATIE
        | INTRAVESIC // INTRAVESIC. INTRAVESICAAL
        | IV // INTRAVENEUS IV
        | NASAL // NASAAL NEUS
        | ORAL // ORAAL GASTR-ENTER OROMUCOSAAL
        | OROMUCOSAL //OROMUCOSAAL
        | RECTAL // RECTAAL
        | SUBCUT // INTRADERMAAL SUBCUTAAN
        | NoRoute

    let routeMapping =
        [ 
            (AUR, ["AURICULAIR"; "OOR"])
            (CUT, [ "CUTAAN"; "TRANSDERMAAL"; "TRANSDERML"; "LOKAAL"])
            (ENDOTR, ["ENDOTR.PULM"; "ENDOTRACHEOPULMONAIR"])
            (EPIDUR, ["EPIDURAAL"])
            (IM, ["INTRAMUSCULAIR"; "IM"])
            (INH, ["INHALATIE"; "INH"])
            (INTRAVESIC, ["INTRAVESIC."; "INTRAVESICAAL"])
            (IV, ["INTRAVENEUS"; "IV"])
            (NASAL, ["NASAAL"; "NEUS"])
            (ORAL, ["ORAAL"; "GASTR-ENTER"; "OR"])
            (OROMUCOSAL, ["OROMUCOSAAL"])
            (RECTAL, ["RECTAAL"; "RECT"])
            (SUBCUT, ["INTRADERMAAL"; "SUBCUTAAN"; "SC"])
        ]

    let createRoute s = 
        let m = 
            routeMapping 
            |> List.tryFind (fun (_, rs) -> 
                rs 
                |> List.exists (String.equalsCapInsens s)
            )
        match m with
        | Some (r, _) -> r
        | _ -> NoRoute

    let eqsRoute r s = s |> createRoute = r

    type Age = float Option

    type Weight = float Option
    
    type BSA = float Option

    let inRange n { DoseRule.Min = min; DoseRule.Max = max } =
        if n |> Option.isNone then true
        else
            let n = n |> Option.get
            match min, max with
            | None, None -> true
            | Some min, None -> n >= min
            | None, Some max -> n <= max
            | Some min, Some max -> n >= min && n <= max
        
    type Filter =
        {
            Age: Age
            Weight: Weight
            BSA: BSA
            GPK: int Option
            Generic: string
            Shape: string
            Route: string
        }

    let createFilter age wght bsa gpk gen shp rte =
        {
            Age = age
            Weight = wght
            BSA = bsa
            GPK = gpk
            Generic = gen
            Shape = shp
            Route = rte
        }

    let find (filter: Filter) =
        let r = filter.Route |> createRoute 
        if r = NoRoute then Array.empty
        else
            match filter.GPK with
            | Some gpk -> [|gpk|]
            | None ->
                GenPresProduct.filter filter.Generic filter.Shape filter.Route
                |> Array.collect (fun gpp -> 
                    gpp.GenericProducts 
                    |> Array.map (fun gp -> gp.Id)
                )
            |> Array.collect (fun gpk ->
                DoseRule.get()
                |> Array.filter (fun dr -> 
                    dr.CareGroup = "intensieve"
                    && dr.GenericProduct |> Array.exists (fun gp -> gp.Id = gpk)
                    && dr.Route |> eqsRoute r
                    && dr.Age |> inRange filter.Age
                    && dr.Weight |> inRange filter.Weight
                    && dr.BSA |> inRange filter.BSA
                )
                |> Array.distinct
            )

    // stuk
    // ml
    // dosis
    // g
    // mg
    // ug
    // milj. IE
    // IE
    // E
    type RuleResult =
        {
            DoseRules: string []
            Doses: FreqDose []
        }
    and FreqDose =
        {
            Freq: DoseRule.Frequency
            NormDose: DoseRule.MinMax
            AbsDose: DoseRule.MinMax
            NormKg: DoseRule.MinMax
            AbsKg: DoseRule.MinMax
            NormM2: DoseRule.MinMax
            AbsM2: DoseRule.MinMax
        }

    let createResult drs ds =
        {
            DoseRules = drs
            Doses = ds
        }

    let createFreqDose freq norm abs normKg absKg normM2 absM2 =
        {
            Freq = freq
            NormDose = norm
            AbsDose = abs
            NormKg = normKg
            AbsKg = absKg
            NormM2 = normM2
            AbsM2 = absM2
        }
     

    let convertToResult (drs : DoseRule.DoseRule  []) =

        let optionChoose cp x1 x2 = 
            match x1, x2 with
            | None, None -> None
            | Some _, None -> x1
            | None, Some _ -> x2
            | Some x1', Some x2' -> if cp x1' x2' then x1' |> Some else x2' |> Some

        let optionMin = optionChoose (<=)

        let optionMax = optionChoose (>=)

        let foldMinMax xs = 
            xs |> Array.fold (fun { DoseRule.Min = min; DoseRule.Max = max} (acc: DoseRule.MinMax) ->
                { Min = optionMin acc.Min min; Max = optionMax acc.Max max }
            ) { DoseRule.Min = None; DoseRule.Max = None }

        let multMinMax f n { DoseRule.Min = min; DoseRule.Max = max } =
            let m = f * n
            match min, max with
            | None, None -> { DoseRule.Min = min; DoseRule.Max = max}
            | Some min', None -> { Min = (min' * m |> Some); Max = max}
            | None, Some max' -> { Min = None; Max = (max' * m |> Some)}
            | Some min', Some max' -> { Min = (min' * m |> Some); Max = (max' * m |> Some)}

        let gpks (dr : DoseRule.DoseRule) = 
            dr.GenericProduct 
            |> Array.map (fun gp -> gp.Id) 
            |> Array.toList
            |> GenericProduct.get

        let norm drs' = 
            drs'
            |> Array.collect (fun dr -> 
                dr
                |> gpks
                |> Array.map (fun gp ->
                    let n = 
                        (gp.Substances
                        |> Array.head).SubstanceQuantity
                    dr.Norm |> multMinMax dr.Freq.Frequency n)
                )
            |> foldMinMax

        let abs drs' = 
            drs'
            |> Array.collect (fun dr -> 
                dr
                |> gpks
                |> Array.map (fun gp ->
                    let n = 
                        (gp.Substances
                        |> Array.head).SubstanceQuantity
                    dr.Abs |> multMinMax dr.Freq.Frequency n)
                )
            |> foldMinMax

        let normKg drs' = 
            drs'
            |> Array.collect (fun dr -> 
                dr
                |> gpks
                |> Array.map (fun gp ->
                    let n = 
                        (gp.Substances
                        |> Array.head).SubstanceQuantity
                    dr.NormKg |> multMinMax dr.Freq.Frequency n)
                )
            |> foldMinMax

        let absKg drs' = 
            drs'
            |> Array.collect (fun dr -> 
                dr
                |> gpks
                |> Array.map (fun gp ->
                    let n = 
                        (gp.Substances
                        |> Array.head).SubstanceQuantity
                    dr.AbsKg |> multMinMax dr.Freq.Frequency n)
                )
            |> foldMinMax

        let normM2 drs' = 
            drs'
            |> Array.collect (fun dr -> 
                dr
                |> gpks
                |> Array.map (fun gp ->
                    let n = 
                        (gp.Substances
                        |> Array.head).SubstanceQuantity
                    dr.NormM2 |> multMinMax dr.Freq.Frequency n)
                )
            |> foldMinMax

        let absM2 drs' = 
            drs'
            |> Array.collect (fun dr -> 
                dr
                |> gpks
                |> Array.map (fun gp ->
                    let n = 
                        (gp.Substances
                        |> Array.head).SubstanceQuantity
                    dr.AbsM2 |> multMinMax dr.Freq.Frequency n)
                )
            |> foldMinMax

        let freqs =
            drs
            |> Array.map (fun dr -> dr.Freq)
            |> Array.distinct
            |> Array.map (fun fr ->
                let drs' =
                    drs
                    |> Array.filter (fun dr -> dr.Freq = fr)
                createFreqDose fr (drs' |> norm) (drs' |> abs) (drs' |> normKg) (drs' |> absKg) (drs' |> normM2) (drs' |> absM2) 
            )

        createResult (drs |> Array.map (DoseRule.toString ", ")) freqs 

