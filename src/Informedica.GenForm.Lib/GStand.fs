namespace Informedica.GenForm.Lib


module GStand =

    open MathNet.Numerics

    open Informedica.GenUtils.Lib
    open Informedica.GenUtils.Lib.BCL

    open Aether
    open Aether.Optics
    open DoseRule
    open DoseRule.Dosage
    open MinMax

    module GPP = Informedica.GenProduct.Lib.GenPresProduct
    module ATC = Informedica.GenProduct.Lib.ATCGroup
    module DR = Informedica.GenProduct.Lib.DoseRule
    module RF = Informedica.GenProduct.Lib.RuleFinder

    module ValueUnit = Informedica.GenUnits.Lib.ValueUnit
    module UNTS = ValueUnit.Units

    type GenPresProduct = GPP.GenPresProduct

    type DoseRule = DoseRule.DoseRule
    type IndicationDosage = DoseRule.IndicationDosage
    type RouteDosage = DoseRule.RouteDosage
    type ShapeDosage = DoseRule.ShapeDosage
    type PatientDosage = DoseRule.PatientDosage
    type SubstanceDosage = DoseRule.Dosage.Dosage
    type Patient = Patient.Patient
    type Dosage = DoseRule.Dosage.Dosage


    let groupByFst xs = 
        xs
        |> Seq.groupBy fst
        |> Seq.sortBy fst
        |> Seq.map (fun (k, v) -> k, v |> Seq.map snd)


    type DoseMapping =
        | Norm
        | Abs
        | NormKg
        | AbsKg
        | NormM2
        | AbsM2


    type UnitMapping = | NoRate | Rate 


    let unitMapping =
        [
            "dopamine", UNTS.perOneHour, NormKg, UNTS.mcgKgMin, Rate
            "fentanyl", UNTS.perOneDay, NormKg,  UNTS.mcgKgDay, NoRate
            "fentanyl", UNTS.perOneHour, NormKg, UNTS.mcgKgHour, Rate
            "salbutamol", UNTS.perFourHour, NormKg, UNTS.mcgKgMin, Rate
        ]


    /// Map GSTand min max float Option values to
    /// a `DoseRule` `MinMax`
    let mapMinMax<'a> 
                  (setMin : float Option -> 'a -> 'a) 
                  (setMax : float Option -> 'a -> 'a) 
                  (minmax : DR.MinMax)
                  (o : 'a) =
        o
        |> setMin minmax.Min
        |> setMax minmax.Max


    // Get the min max weight if there is one min weight or max weight
    let calcWeightMinMax (drs : DR.DoseRule seq) =

        match drs |> Seq.toList with
        | [] -> DR.minmax
        | [ h ] -> h.Weight
        | h::tail ->
            if tail |> List.forall (fun mm -> mm.Weight = h.Weight) then h.Weight
            else DR.minmax
        |> mapMinMax ((Option.bind ValueUnit.weightInKg) >> (Optic.set MinMax.Optics.inclMinLens))
                     ((Option.bind ValueUnit.weightInKg) >> (Optic.set MinMax.Optics.exclMaxLens))


    // Get the min max bsa if there is one min bsa or max bsa
    let calcBSAMinMax (drs : DR.DoseRule seq) =

        match drs |> Seq.toList with
        | [] -> DR.minmax
        | [h] -> h.BSA
        | h::tail ->
            if tail |> List.forall (fun mm -> mm.BSA = h.Weight) then h.BSA
            else DR.minmax
        |> mapMinMax ((Option.bind ValueUnit.bsaInM2) >> (Optic.set MinMax.Optics.inclMinLens))
                     ((Option.bind ValueUnit.bsaInM2) >> (Optic.set MinMax.Optics.exclMaxLens))
        

    /// Make sure that a GSTand time string 
    /// is a valid unit time string
    let parseTimeString s =
        s
        |> String.replace "per " ""
        |> String.replace "dagen" "dag"
        |> String.replace "weken" "week"
        |> String.replace "maanden" "maand"
        |> String.replace "minuten" "minuut"
        |> String.replace "uren" "uur"
        |> String.replace "eenmalig" ""
        |> (fun s -> 
            if s |> String.isNullOrWhiteSpace then s
            else s + "[Time]"
        )


    /// Map a GStand time period to a valid unit
    let mapTime s =
        s
        |> parseTimeString
        |> ValueUnit.Units.fromString


    /// Map GStand frequency string to a valid 
    /// frequency `ValueUnit`.
    let mapFreq (fr: DR.Frequency) =
        let map vu =
            match
                [
                    2N, ValueUnit.freqUnitPerNday 3N, ValueUnit.freqUnitPerNHour 36N
                ]
                |> List.tryFind (fun (f, u, _) -> f |> ValueUnit.create u = vu) with
            | Some (_, _, c) -> vu |> ValueUnit.convertTo c
            | None -> vu

        let s = fr.Frequency |> string
        let s = s + " X[Count]"

        fr.Time
        |> parseTimeString
        |> (fun s' -> 
            match s' |> String.split " " with
            | [v;u] -> s + "/" + v + " " + u
            | [u]   -> 
                if u |> String.isNullOrWhiteSpace then s
                else 
                    s + "/1" + " " + u
            | _ -> ""
        )
        |> ValueUnit.fromString
        |> map


    /// Map GSTand doserule doses to 
    /// - normal   min max dose
    /// - absolute min max dose
    /// - normal   min max dose per kg
    /// - absolute min max dose per kg
    /// - normal   min max dose per m2
    /// - absolute min max dose per m2
    /// by calculating 
    /// - substance shape concentration * dose shape quantity * frequency
    /// for each dose
    let mapDoses n qty unit (gstdsr : DR.DoseRule) =

        let fr = mapFreq gstdsr.Freq

        let setMin = Optic.set MinMax.Optics.inclMinLens
        let setMax = Optic.set MinMax.Optics.exclMaxLens

        let toVu n mapping v =
            let u = 
                match mapping with
                | NormKg | AbsKg -> unit |> ValueUnit.per ValueUnit.Units.Weight.kiloGram
                | NormM2 | AbsM2 -> unit |> ValueUnit.per ValueUnit.Units.BSA.M2
                | _ -> unit

            match u |> ValueUnit.fromFloat (v * qty) with
            | Some vu -> 
                vu * fr 
                |> (fun vu ->
                    let _, u = fr |> ValueUnit.get

                    match unitMapping 
                          |> List.tryFind (fun (n_, tu, m, _, _) -> 
                            n_ |> String.equalsCapInsens n && 
                            m = mapping &&
                            tu = u
                          ) with
                    | None -> vu
                    | Some (_, _, _, cu, _) ->
                        vu |> ValueUnit.convertTo cu)
                |> Some
            | None -> None
    
        let minmax n mapping (mm : DR.MinMax) =
            MinMax.empty
            |> setMin (mm.Min |> Option.bind (toVu n mapping))
            |> setMax (mm.Max |> Option.bind (toVu n mapping))
             
        //printfn "mapping doserule: \n%s to\n %s x %s" 
        //    (gstdsr |> Informedica.GenProduct.Lib.DoseRule.toString2)
        //    (fr |> ValueUnit.toStringPrec 1)
        //    (gstdsr.NormKg |> minmax n NormKg |> MinMax.toString)

        (n, gstdsr.Freq.Time |> parseTimeString, gstdsr.Freq.Frequency = 1.) ,
        
        (gstdsr.Routes |> Array.toList,
         gstdsr.Indication,
         fr,
         gstdsr.Norm   |> minmax n Norm,
         gstdsr.Abs    |> minmax n Abs,
         gstdsr.NormKg |> minmax n NormKg,
         gstdsr.AbsKg  |> minmax n AbsKg,
         gstdsr.NormM2 |> minmax n NormM2,
         gstdsr.AbsM2  |> minmax n AbsM2,
         gstdsr)

          

    let getSubstanceDoses (drs : DR.DoseRule seq) =
        // fold maximize with preservation of min
        let fold (mm : MinMax) (mm_ : MinMax) =
            match mm.Min, mm.Min with
            | Some m, None 
            | None, Some m -> [ mm |> MinMax.setMin ( Some m); mm_ |> MinMax.setMin (Some m) ]
            | _ -> [ mm; mm_ ]
            |> MinMax.foldMaximize

        drs 
        |> Seq.collect (fun dr ->
            dr.GenericProduct
            |> Seq.collect (fun gp ->
                gp.Substances
                |> Seq.collect (fun s ->
                    match s.Unit |> ValueUnit.unitFromString Mapping.GStandMap with
                    | None -> []
                    | Some u ->
                        [ mapDoses s.Name s.Quantity u dr ]
                )
            )
        )
        |> Seq.groupBy fst // group by substance name frequency time and whether frequency = 1
        |> Seq.map ((fun (k, v) -> 
            k , 
            v 
            |> Seq.fold (fun acc (_, (rts, ind, fr, norm, abs, normKg, absKg, normM2, absM2, gstdsr)) -> 
                let _, inds, frs, gstdsrs, norm_, abs_, normKg_, absKg_, normM2_, absM2_ = acc

                let frs =
                    let tu = fr |> ValueUnit.get |> snd
                    
                    if frs |> List.exists (fun fr_ ->
                        let u_ = fr_ |> ValueUnit.get |> snd
                       
                        fr_ |> ValueUnit.get |> snd <> tu
                    ) then 
                        let s1 = fr |> ValueUnit.toStringPrec 1
                        let s2 = 
                            frs
                            |> List.map (ValueUnit.toStringPrec 1)
                            |> String.concat ", "

                        failwith <| sprintf "cannot add frequency %s to list with units %s" s1 s2


                    if frs |> List.exists ((=) fr) then frs else frs @ [ fr ]

                let inds =
                    if inds |> List.exists ((=) ind) then inds else inds @ [ ind ]

                let gstdsrs = gstdsr::gstdsrs

                let norm = fold norm norm_ 
                let abs = fold abs abs_ 
                let normKg = fold normKg normKg_ 
                let absKg = fold absKg absKg_ 
                let normM2 = fold normM2 normM2_ 
                let absM2 = fold absM2 absM2_ 

                rts, inds, frs, gstdsrs, norm, abs, normKg, absKg, normM2, absM2
            ) ([], [], [], [], MinMax.empty, MinMax.empty, MinMax.empty, MinMax.empty, MinMax.empty, MinMax.empty)) 
        >> ((fun (k, (rts, inds, frs, gstdsrs, norm, abs, normKg, absKg, normM2, absM2)) ->
            let w = MinMax.empty |> calcWeightMinMax gstdsrs
            let b = MinMax.empty |> calcBSAMinMax gstdsrs
            
            // if weight or bsa is known the adjusted or unadjusted doses can be calculated
            let calcNoneAndAdjusted (c : MinMax) (un : MinMax) (adj : MinMax) =
                let calc op x1 x2 y =
                    match y with
                    | Some _ -> y
                    | None -> 
                        match x1, x2 with
                        | Some x1_, Some x2_ ->
                            // printfn "calculating %A %A = %A" x1_ x2_ (x1_ |> op <| x2_)  
                            (x1_ |> op <| x2_) |> Some
                        | _ -> y

                // Norm.min = PerKg.min * Wght.min
                // Norm.max = PerKg.max * Wght.max
                { un with 
                    Min = un.Min |> calc (*) adj.Min c.Min
                    Max = un.Max |> calc (*) adj.Max c.Max } ,
                // PerKg.min = Norm.min / Wght.max
                // PerKg.max = norm.max / Wght.min
                { adj with
                    Min = adj.Min |> calc (/) un.Min c.Max
                    Max = adj.Max |> calc (/) un.Max c.Min }

            k ,
            rts,
            inds ,
            frs ,
            gstdsrs ,
            DoseRule.DoseRange.create 
                (calcNoneAndAdjusted w norm normKg |> fst)
                (calcNoneAndAdjusted w norm normKg |> snd, ValueUnit.Units.Weight.kiloGram) 
                (calcNoneAndAdjusted b norm normM2 |> snd, ValueUnit.Units.BSA.M2)
                (calcNoneAndAdjusted w abs  absKg |> fst) 
                (calcNoneAndAdjusted w abs  absKg |> snd, ValueUnit.Units.Weight.kiloGram) 
                (calcNoneAndAdjusted b abs  absM2 |> snd, ValueUnit.Units.BSA.M2)) 
        >> (fun ((n, _, _), rts, inds, frs, gstdsrs, dr)  ->
            
            let tu = 
                match frs with
                | fr::_ -> fr |> ValueUnit.get |> snd
                | _ -> ValueUnit.NoUnit

            inds ,
            DoseRule.Dosage.empty
            |> (Optic.set Dosage.Name_ n)  
            |> (Optic.set Dosage.Rules_ (gstdsrs |> List.map (DR.toString2 >> Dosage.GStandRule)))
            |> (fun ds ->
                match tu with
                | _ when tu = ValueUnit.NoUnit || (tu |> ValueUnit.isCountUnit) -> 
                    ds 
                    |> (Optic.set Dosage.SingleDosage_ dr) 

                | _ when rts = ["INTRAVENEUS"] &&
                         unitMapping 
                         |> List.exists (fun (n_, u, _, _, dt) ->
                            n_ |> String.equalsCapInsens n && 
                            tu = u &&
                            dt = Rate
                         ) ->
                    
                    ds
                    |> (Optic.set Dosage.RateDosage_ (dr, tu))

                | _  ->
                    let frs =
                        let fr = 
                            frs 
                            |> List.map ValueUnit.getValue
                            |> List.sort
                        Dosage.createFrequency fr tu None         
                    
                    ds
                    |> (Optic.set Dosage.TotalDosage_ (dr, tu))
                    |> (Optic.set Dosage.Frequencies_ frs)
            )))) 


    let getPatients (drs : DR.DoseRule seq) =
        let map = mapMinMax<Patient> 

        let ageInMo = Option.bind ValueUnit.ageInMo

        let wghtKg = Option.bind ValueUnit.weightInKg

        let mapAge = 
            map (ageInMo >> Patient.Optics.setInclMinAge) 
                (ageInMo >> Patient.Optics.setExclMaxAge) 

        let mapWght =
            map (wghtKg >> Patient.Optics.setInclMinWeight) 
                (wghtKg >> Patient.Optics.setInclMaxWeight) 

        let mapGender s =
            match s with
            | _ when s = "man" -> Patient.Male
            | _ when s = "vrouw" -> Patient.Female
            | _ -> Patient.Undetermined
            |> (Optic.set Patient.Gender_)
        
        drs
        |> Seq.map (fun dr ->
            (dr.Indication ,
             Patient.empty
             |> mapAge dr.Age
             |> mapWght dr.Weight
             |> mapGender dr.Gender) , dr
        )
        |> Seq.groupBy fst
        |> Seq.map ((fun (k, v) -> k |> snd, v |> Seq.map snd) 
        >> (fun (pat, drs) ->
            (pat, drs |> getSubstanceDoses, drs))
        ) 


    // Get the ATC codes for a GenPresProduct
    let getATCs (gpp : GenPresProduct) =
        gpp.GenericProducts
        |> Array.map(fun gp -> gp.ATC)
        |> Array.distinct


    // Get the list of routes for a GenPresProduct
    let getRoutes (gpp: GenPresProduct) = 
        gpp.GenericProducts
        |> Array.collect (fun gp -> gp.Route)
        |> Array.distinct


    // Get the list of ATC groups for a GenPresProduct
    let getATCGroups all (gpp: GenPresProduct) =
        ATC.get all
        |> Array.filter (fun g -> 
            gpp
            |> getATCs
            |> Array.exists (fun a -> 
                a |> String.equalsCapInsens g.ATC5) && g.Shape = gpp.Shape
            )
        |> Array.distinct


    // Get the doserules for a genpresproduct
    // ToDo Temp hack ignore route and shape
    let getDoseRules all (gpp : GenPresProduct) =
        gpp.Route
        |> Seq.collect (fun r ->
            RF.createFilter None None None None gpp.Name gpp.Shape r
            |> RF.find all
            |> Seq.map (fun dr -> dr.Indication, (r, dr))
        )
        |> Seq.groupBy fst

    
    let getTradeNames (gpp : GenPresProduct) =
        gpp.GenericProducts
        |> Seq.collect (fun gp -> gp.PrescriptionProducts)
        |> Seq.collect (fun pp -> pp.TradeProducts)
        |> Seq.map (fun tp -> 
            match tp.Name |> String.split " " with
            | h::_ -> h |> String.trim
            | _ -> ""
        )
        |> Seq.filter (fun n -> n |> String.isNullOrWhiteSpace |> not)
        |> Seq.toList

   
    let createDoseRules all age wght bsa gpk gen shp rte =
        
        GPP.filter all gen shp rte
        |> Seq.collect (fun gpp ->
            gpp 
            |> getATCGroups ()
            |> Seq.map (fun atc -> 
                (atc.Generic, 
                    atc.ATC5, 
                    atc.TherapeuticMainGroup, 
                    atc.TherapeuticSubGroup, 
                    atc.PharmacologicalGroup, 
                    atc.Substance) ,
                gpp
            )
        )
        |> groupByFst
        |> Seq.map ((fun (k, v) ->
            let gen, atc, tg, tsg, pg, sg = k
            // create empty dose rule
            DoseRule.create gen [] atc tg tsg pg sg [] ,
            v) >> ((fun (dr, gpps) ->
            dr
            |> DoseRule.Optics.setSynonyms (gpps |> Seq.collect getTradeNames |> Seq.toList) ,
            gpps
            |> Seq.collect (fun (gpp : GenPresProduct) ->
                gpp.Route
                |> Seq.filter (fun r -> rte |> String.isNullOrWhiteSpace || r |> String.equalsCapInsens rte)
                |> Seq.collect (fun r ->
                    RF.createFilter age wght bsa gpk gpp.Name gpp.Shape r
                    |> RF.find all
                    |> getPatients
                    |> Seq.collect (fun (pat, sds, dsrs) ->
                        let gps = dsrs |> Seq.collect (fun dr -> dr.GenericProduct |> Seq.map (fun gp -> gp.Name))
                        let tps = dsrs |> Seq.collect (fun dr -> dr.TradeProduct |> Seq.map (fun tp -> tp.Name))

                        sds
                        |> Seq.map (fun (ind, sds) -> ind, (r, (gpp.Shape, gps, tps, pat, sds)))
                    )
                )
            )
            |> groupByFst // group by indications
            |> Seq.map (fun (k, v) -> 
                k, 
                v
                |> groupByFst // group by route
                |> Seq.map (fun (k, v) -> 
                    k, 
                    v
                    |> Seq.map (fun (shp, gps, tps, pat, sds) -> shp, gps, tps, pat, sds)
                    |> Seq.groupBy (fun (shp, gps, tps, _, _)  -> (shp, gps, tps)) // group by shape and products
                    |> Seq.sortBy (fst >> (fun (shp, _, _) -> shp))
                    |> Seq.map (fun (k, v) -> 
                        k, 
                        v
                        |> Seq.map (fun (_, _, _, pat, sds) -> pat, sds)
                        |> groupByFst // group by patient
                    )
                )
            // add indications, route, shape, patient and dosages
            )) >> (fun (dr, inds) ->
            inds
            |> Seq.fold (fun acc ind ->
                let ind, rts = ind

                let dr =
                    acc
                    |> DoseRule.addIndications ind

                rts 
                |> Seq.fold (fun acc rt ->
                    let r, shps = rt

                    let dr =
                        acc 
                        |> DoseRule.Optics.addRoute ind r
                
                    shps 
                    |> Seq.fold (fun acc shp ->
                        let (shp, gps, tps) , pats = shp

                        let dr =
                            acc
                            |> DoseRule.Optics.addShape ind r [shp]
                            |> DoseRule.Optics.setGenericProducts ind r [shp] (gps |> Seq.toList |> List.sort)
                            |> DoseRule.Optics.setTradeProducts ind r [shp] (tps |> Seq.toList |> List.sort)
                    
                        pats
                        |> Seq.fold (fun acc pat ->
                            let pat, sds = pat

                            acc
                            |> DoseRule.Optics.addPatient ind r [shp] pat
                            |> DoseRule.Optics.setSubstanceDosages ind r [shp] pat (sds |> Seq.toList)
                        ) dr
                    ) dr
                ) dr
            ) dr))) 