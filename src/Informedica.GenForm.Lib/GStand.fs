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
    open System.Drawing

    module GPP = Informedica.GenProduct.Lib.GenPresProduct
    module ATC = Informedica.GenProduct.Lib.ATCGroup
    module DR = Informedica.GenProduct.Lib.DoseRule
    module RF = Informedica.GenProduct.Lib.RuleFinder
    

    module ValueUnit = Informedica.GenUnits.Lib.ValueUnit
    module UNTS = ValueUnit.Units

    type GenPresProduct = GPP.GenPresProduct

    type DoseRule = DoseRule.DoseRule
    type IndicationDosage = DoseRule.IndicationDosage
    type RouteDosage = DoseRule.RouteDosage.RouteDosage
    type ShapeDosage = DoseRule.ShapeDosage.ShapeDosage
    type TradeProduct = DoseRule.ShapeDosage.TradeProduct
    type GenericProduct = DoseRule.ShapeDosage.GenericProduct
    type PatientDosage = DoseRule.PatientDosage.PatientDosage
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

   
    type CreateConfig =
        {
            UseAll : bool
            IsRate : bool
            SubstanceUnit : ValueUnit.Unit Option
            TimeUnit : ValueUnit.Unit Option
        }



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

        let s = 
            match fr.Frequency |> BigRational.fromFloat with
            | Some br -> br |> string
            | None -> ""
        let s = s + " X[Count]"

        fr.Time
        |> parseTimeString
        |> (fun s' -> 
            match s' |> String.trim |> String.split " " with
            | [v;u] -> 
                let br =
                    match v |> Double.tryParse with
                    | Some d -> 
                        match d |> BigRational.fromFloat with 
                        | Some s ->  s |> string
                        | None -> ""
                    | None -> ""
                s + "/" + br + " " + u
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

        // ToDo remove n and mapping
        let toVu n mapping v =

            match unit |> ValueUnit.fromFloat (v * qty) with
            | Some vu -> 
                let x = 
                    fr 
                    |> ValueUnit.get
                    |> fst
                    |> ValueUnit.create ValueUnit.Units.Count.times
                vu * x |> Some
            | None -> None
    
        let minmax n mapping (mm : DR.MinMax) =
            MinMax.empty
            |> setMin (mm.Min |> Option.bind (toVu n mapping))
            |> setMax (mm.Max |> Option.bind (toVu n mapping))
             

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
          

    let getSubstanceDoses (cfg : CreateConfig) (drs : DR.DoseRule seq) =
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
                    match s.Unit |> ValueUnit.unitFromMappedString Mapping.GStandMap with
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
                // remove the adjust unit by making it a count
                let c = 
                    c |> MinMax.withUnit ValueUnit.Units.Count.times

                let calc op x1 x2 y =
                    match y with
                    | Some _ -> y
                    | None -> 
                        match x1, x2 with
                        | Some x1_, Some x2_ ->
                            // printfn "calculating %A %A = %A" x1_ x2_ (x1_ |> op <| x2_)  
                            (x1_ |> op <| x2_) 
                            |> Some
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
                | fr::_ -> 
                    match fr |> ValueUnit.get |> snd with
                    | ValueUnit.CombiUnit(_, ValueUnit.OpPer, tu) -> tu
                    | _ -> ValueUnit.NoUnit
                | _ -> ValueUnit.NoUnit

            inds ,
            DoseRule.Dosage.empty
            |> (Optic.set Dosage.Name_ n)  
            |> (Optic.set Dosage.Rules_ (gstdsrs |> List.map (DR.toString2 >> Dosage.GStandRule)))
            |> (fun ds ->
                match tu with
                | _ when tu = ValueUnit.NoUnit || (tu |> ValueUnit.isCountUnit) -> 
                    ds 
                    |> (Optic.set Dosage.StartDosage_ dr) 

                | _ when cfg.IsRate && 
                         frs |> List.length = 1 &&
                         tu = ValueUnit.Units.Time.hour ->

                    ds
                    |> (Optic.set Dosage.RateDosage_ (dr, tu))
                    |> (fun ds ->
                        match cfg.TimeUnit with
                        | Some u ->
                            ds
                            |> Dosage.convertRateUnitTo u
                        | None -> ds
                    )

                | _  ->
                    let frs =
                        let fr = 
                            frs 
                            |> List.map ValueUnit.getValue
                            |> List.sort
                        Dosage.createFrequency fr tu None         
                    
                    ds
                    |> (Optic.set Dosage.TotalDosage_ (dr, frs))
                // Perform unit conversion
                |> (fun ds ->
                    match cfg.SubstanceUnit with 
                    | Some u -> 
                        ds
                        |> Dosage.convertSubstanceUnitTo u 
                    | None -> ds
               
                )
            )))) 


    let getPatients (cfg : CreateConfig) (drs : DR.DoseRule seq) =
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
            //printfn "patient: %s" (pat |> Patient.toString)
            //printfn "doserules:\n%s" (drs |> Seq.map DR.toString2 |> String.concat "\n")

            (pat, drs |> getSubstanceDoses cfg, drs))
        ) 


    // Get the ATC codes for a GenPresProduct
    let getATCs gpk (gpp : GenPresProduct) =
        gpp.GenericProducts
        |> Array.filter (fun gp ->
            match gpk with
            | None -> true
            | Some id -> gp.Id = id
        )
        |> Array.map(fun gp -> gp.ATC)
        |> Array.distinct


    // Get the list of routes for a GenPresProduct
    let getRoutes (gpp: GenPresProduct) = 
        gpp.GenericProducts
        |> Array.collect (fun gp -> gp.Route)
        |> Array.distinct


    // Get the list of ATC groups for a GenPresProduct
    let getATCGroups gpk (gpp: GenPresProduct) =
        
        ATC.get ()
        |> Array.filter (fun g -> 
            gpp
            |> getATCs gpk
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


    let mergeDosages d ds =
        
        let merge d1 d2 =
            //printfn "merging d1: %s" (d1 |> Dosage.toString false)
            //printfn "merging d2: %s" (d2 |> Dosage.toString false)

            Dosage.empty
            // merge name
            |> (fun d ->
                d
                |> Dosage.Optics.setName (d1 |> Dosage.Optics.getName)
            )
            // merge start dose
            |> (fun d ->
                if d.StartDosage = DoseRange.empty then
                    if d1.StartDosage = DoseRange.empty then d2.StartDosage else d1.StartDosage
                    |> (fun x -> d |> (Optic.set Dosage.StartDosage_ x))
                else d
            )
            // merge single dose
            |> (fun d ->
                if d.SingleDosage = DoseRange.empty then
                    if d1.SingleDosage = DoseRange.empty then d2.SingleDosage else d1.SingleDosage
                    |> (fun x -> d |> (Optic.set Dosage.SingleDosage_ x))
                else d
            )
            // merge Rate dose
            |> (fun d ->
                if d.RateDosage |> fst = DoseRange.empty then
                    if d1.RateDosage |> fst = DoseRange.empty then d2.RateDosage else d1.RateDosage
                    |> (fun x -> d |> (Optic.set Dosage.RateDosage_ x))
                else d
            )
            // merge frequencies when freq is 1 then check
            // whether the freq is a start dose
            |> (fun d -> 
                // only merge frequencies for same total dose
                // ToDo use ValueUnit eqs function
                if d1 |> (Optic.get Dosage.TotalDosage_) = (d2 |> Optic.get Dosage.TotalDosage_) ||
                   d1 |> (Optic.get Dosage.TotalDosage_) |> fst = DoseRange.empty || 
                   d2 |> (Optic.get Dosage.TotalDosage_) |> fst = DoseRange.empty then
                    d1 
                    |> Dosage.Optics.getFrequencyValues
                    |> List.append (d2 |> Dosage.Optics.getFrequencyValues)
                    |> (fun vs -> 
                        d
                        |> Dosage.Optics.setFrequencyValues vs
                    )
                    // merge Total dose
                    |> (fun d ->
                        if d.TotalDosage |> fst = DoseRange.empty then
                            if d1.TotalDosage |> fst = DoseRange.empty then d2.TotalDosage else d1.TotalDosage
                            |> (fun x -> d |> (Optic.set Dosage.TotalDosage_ x))
                        else d
                    )

                else
                    if d1 |> Dosage.Optics.getFrequencyValues = [ 1N ] then
                        d
                        |> (Optic.set Dosage.SingleDosage_ (d1.TotalDosage |> fst))
                        |> (fun d ->
                            d2
                            |> Dosage.Optics.getFrequencyValues
                            |> (fun vs ->
                                d
                                |> Dosage.Optics.setFrequencyValues vs
                                |> (Optic.set Dosage.TotalDosage_ (d2 |> Optic.get Dosage.TotalDosage_))
                            )
                        )
                    else if d2 |> Dosage.Optics.getFrequencyValues = [ 1N ] then
                        d
                        |> (Optic.set Dosage.SingleDosage_ (d2.TotalDosage |> fst))
                        |> (fun d ->
                            d1
                            |> Dosage.Optics.getFrequencyValues
                            |> (fun vs ->
                                d
                                |> Dosage.Optics.setFrequencyValues vs
                                |> (Optic.set Dosage.TotalDosage_ (d1 |> Optic.get Dosage.TotalDosage_))
                            )
                        )
                    else
                        d

            )

            |> (fun d -> 
                d 
                |> Dosage.Optics.setFrequencyTimeUnit (d1 |> Dosage.Optics.getFrequencyTimeUnit)
            )
            //|> (fun d -> 
            //    printfn "dosage is now: %s" (d |> Dosage.toString false)
            //    d
            //)

        match ds |> Seq.toList with
        | [d1] -> seq { yield merge d1 d }
        | _ -> 
            ds
            |> Seq.append (seq { yield d })
        

    let createDoseRules (cfg : CreateConfig) age wght bsa gpk gen shp rte =
        
        GPP.filter cfg.UseAll gen shp rte
        |> Seq.filter (fun gpp ->
            match gpk with
            | None -> true
            | Some id ->
                gpp.GenericProducts
                |> Seq.exists (fun gp -> gp.Id = id)
        )
        |> Seq.collect (fun gpp ->
            gpp 
            |> getATCGroups gpk
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
            DoseRule.create gen [] atc tg tsg pg sg [] , v) 
            >> ((fun (dr, gpps) ->
                dr
                |> DoseRule.Optics.setSynonyms (gpps |> Seq.collect getTradeNames |> Seq.toList) ,
                gpps
                |> Seq.collect (fun (gpp : GenPresProduct) ->
                    gpp.Route
                    |> Seq.filter (fun r -> rte |> String.isNullOrWhiteSpace || r |> String.equalsCapInsens rte)
                    |> Seq.collect (fun r ->
                        RF.createFilter age wght bsa gpk gpp.Name gpp.Shape r
                        |> RF.find cfg.UseAll
                        |> getPatients cfg
                        |> Seq.collect (fun (pat, sds, dsrs) ->
                            let gps = dsrs |> Seq.collect (fun dr -> dr.GenericProduct |> Seq.map (fun gp -> gp.Id, gp.Name))
                            let tps = dsrs |> Seq.collect (fun dr -> dr.TradeProduct |> Seq.map (fun tp   -> tp.Id, tp.Name))

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
                )
            ) 
            // add indications, route, shape, patient and dosages
            >> (fun (dr, inds) ->
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

                        let createGP = DoseRule.ShapeDosage.GenericProduct.create
                        let createTP = DoseRule.ShapeDosage.TradeProduct.create

                        let dr =
                            acc
                            |> DoseRule.Optics.addShape ind r [shp]
                            |> DoseRule.Optics.setGenericProducts 
                                ind r [shp] 
                                (gps 
                                 |> Seq.toList 
                                 |> List.map (fun (id, nm) -> createGP id nm) 
                                 |> List.sortBy (fun gp -> gp.Label))
                            |> DoseRule.Optics.setTradeProducts 
                                ind r [shp] 
                                (tps 
                                 |> Seq.toList 
                                 |> List.map (fun (id, nm) -> createTP id nm) 
                                 |>  List.sortBy (fun hp -> hp.Label))
                    
                        pats
                        |> Seq.fold (fun acc pat ->
                            let pat, sds = pat

                            let sds =
                                sds
                                |> Seq.fold (fun acc sd ->
                                    match acc 
                                          |> Seq.toList 
                                          |> List.filter (fun d -> d.Name = sd.Name) with
                                    | [] -> acc |> Seq.append (seq { yield sd })
                                    | ns -> 
                                        match ns 
                                              |> List.filter (fun d -> 
                                                d |> Dosage.Optics.getFrequencyTimeUnit = (sd |> Dosage.Optics.getFrequencyTimeUnit) ||
                                                sd |> Dosage.Optics.getFrequencyValues = []
                                              ) with
                                        | [] -> acc |> Seq.append (seq {yield sd })
                                        | ns ->
                                            acc |> mergeDosages sd 
                                            
                                ) Seq.empty

                            acc
                            |> DoseRule.Optics.addPatient ind r [shp] pat
                            |> DoseRule.Optics.setSubstanceDosages ind r [shp] pat (sds |> Seq.toList)
                        ) dr
                    ) dr
                ) dr
            ) dr))) 
