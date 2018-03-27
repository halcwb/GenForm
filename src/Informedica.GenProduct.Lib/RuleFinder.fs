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

    type PatientFilter =
        {
            Age: Age
            Weight: Weight
            BSA: BSA
        }

    type ProductFilter =
        | GPKRoute of (int * string)
        | GenericShapeRoute of GenericShapeRoute
    and GenericShapeRoute = 
        {
            Generic: string
            Shape: string
            Route: string
        }
    

    type Filter =
        {
            Patient: PatientFilter
            Product: ProductFilter
        }

    let createFilter age wght bsa gpk gen shp rte =
        let pat = { Age = age; Weight = wght; BSA = bsa }
        let prod = 
            if gpk |> Option.isSome then
                (gpk |> Option.get, rte)
                |> GPKRoute
            else
                {
                    Generic = gen
                    Shape = shp
                    Route = rte
                } |> GenericShapeRoute
        {
            Patient = pat
            Product = prod
        }
    let createGPKRouteFilter gpk rte = createFilter None None None gpk "" "" rte

    let find { Patient = pat; Product = prod } =
        let r = 
            match prod with
            | GPKRoute (_, route)       -> route
            | GenericShapeRoute gsr -> gsr.Route 
            |> createRoute 

        if r = NoRoute then Array.empty
        else
            match prod with
            | GPKRoute (gpk, _) -> [| gpk |]
            | GenericShapeRoute gsr ->
                GenPresProduct.filter gsr.Generic gsr.Shape gsr.Route
                |> Array.collect (fun gpp -> 
                    gpp.GenericProducts 
                    |> Array.map (fun gp -> gp.Id)
                )
            |> Array.collect (fun gpk ->
                DoseRule.get()
                |> Array.filter (fun dr -> 
                    dr.CareGroup = "intensieve"
                    && dr.GenericProduct |> Array.exists (fun gp -> gp.Id = gpk)
                    && dr.Route  |> eqsRoute r
                    && dr.Age    |> inRange pat.Age
                    && dr.Weight |> inRange pat.Weight
                    && dr.BSA    |> inRange pat.BSA
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
            Product: GenPresProduct.GenPresProduct
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
            Unit: string
        }

    let createResult gpp drs ds =
        {
            Product = gpp
            DoseRules = drs
            Doses = ds
        }

    let createFreqDose freq norm abs normKg absKg normM2 absM2 un =
        {
            Freq = freq
            NormDose = norm
            AbsDose = abs
            NormKg = normKg
            AbsKg = absKg
            NormM2 = normM2
            AbsM2 = absM2
            Unit = un
        }
     

    let convertToResult (drs : DoseRule.DoseRule  []) =
        // Alle dose rules should apply to the same 
        // GenPresProduct
        let gpp =
            drs
            |> Array.collect (fun dr ->
                dr.GenericProduct
                |> Array.map (fun gp -> gp.Id)
            )
            |> Array.distinct
            |> Array.collect GenPresProduct.findByGPK
            |> (fun gpps ->
                if gpps |> Array.isEmpty then None
                else
                    gpps
                    |> Array.fold (fun acc gpp -> 
                        match acc with
                        | Some gpp' -> if gpp' = gpp then acc else None
                        | None -> None
                    ) (Some gpps.[0])
            ) 
            |> (fun r -> printfn "Unique gpp %A" r; r)

        match gpp with
        | Some gpp' ->
            let multMinMax f n { DoseRule.Min = min; DoseRule.Max = max } =
                let m = f * n

                let mn, mx = 
                    match min, max with
                    | None, None           -> (0., 0.)
                    | Some min', None      -> (min' * m, 0.)
                    | None, Some max'      -> (0., max' * m )
                    | Some min', Some max' -> (min' * m, max' * m)

                DoseRule.createMinMax mn mx

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
                |> DoseRule.foldMinMax

            let abs drs' = 
                drs'
                |> Array.collect (fun dr -> 
                    dr
                    |> gpks
                    |> Array.map (fun gp ->
                        let n = 
                            (gp.Substances
                            |> Array.head).SubstanceQuantity
                        dr.Abs |> multMinMax dr.Freq.Frequency n
                    )
                )
                |> DoseRule.foldMinMax

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
                |> DoseRule.foldMinMax

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
                |> DoseRule.foldMinMax

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
                |> DoseRule.foldMinMax

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
                |> DoseRule.foldMinMax

            let un drs' =
                drs'
                |> Array.fold (fun acc dr ->
                    dr
                    |> gpks
                    |> Array.fold (fun acc' gp ->
                        gp.Substances
                        |> Array.fold (fun acc'' s ->
                            if acc'' = "" then s.SubstanceUnit
                            else
                                if acc'' <> s.SubstanceUnit then "_"
                                else s.SubstanceUnit
                        ) acc'
                    ) acc
                ) ""
                |> (fun u -> if u = "_" then "" else u)

            let freqs =
                drs
                |> Array.map (fun dr -> dr.Freq)
                |> Array.distinct
                |> Array.map (fun fr ->
                    let drs' =
                        drs
                        |> Array.filter (fun dr -> dr.Freq = fr)
                    createFreqDose fr (drs' |> norm) (drs' |> abs) (drs' |> normKg) (drs' |> absKg) (drs' |> normM2) (drs' |> absM2) (drs' |> un)
                )

            createResult gpp' (drs |> Array.map (DoseRule.toString ", ")) freqs 
            |> Some
        
        | None -> None

