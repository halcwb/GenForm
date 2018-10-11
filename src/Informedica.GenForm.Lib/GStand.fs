namespace Informedica.GenForm.Lib


module GStand =

    open Informedica.GenUtils.Lib
    open Informedica.GenUtils.Lib.BCL

    open Aether
    open Aether.Optics

    module GPP = Informedica.GenProduct.Lib.GenPresProduct
    module ATC = Informedica.GenProduct.Lib.ATCGroup
    module DR = Informedica.GenProduct.Lib.DoseRule
    module RF = Informedica.GenProduct.Lib.RuleFinder

    module ValueUnit = Informedica.GenUnits.Lib.ValueUnit

    type GenPresProduct = GPP.GenPresProduct

    type DoseRule = DoseRule.DoseRule
    type IndicationDosage = DoseRule.IndicationDosage
    type RouteDosage = DoseRule.RouteDosage
    type ShapeDosage = DoseRule.ShapeDosage
    type PatientDosage = DoseRule.PatientDosage
    type SubstanceDosage = DoseRule.Dosage.Dosage
    type Patient = Patient.Patient
    type Dosage = DoseRule.Dosage.Dosage

    type FlatDoseRule =
        {
            DoseRule : DoseRule
            IndicationDosage : IndicationDosage
            RouteDosage : RouteDosage
            ShapeDosage : ShapeDosage
            PatientDosage : PatientDosage
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

        let toVu perKg perM2 v =
            let u = 
                if not perKg then unit
                else
                    unit |> ValueUnit.per ValueUnit.Units.Weight.kiloGram

            let u = 
                if not perM2 then u
                else
                    u |> ValueUnit.per ValueUnit.Units.BSA.M2

            match u |> ValueUnit.fromFloat (v * qty) with
            | Some vu -> vu * fr |> Some
            | None -> None
    
        let minmax perKg perM2 (mm : DR.MinMax) =
            MinMax.empty
            |> setMin (mm.Min |> Option.bind (toVu perKg perM2))
            |> setMax (mm.Max |> Option.bind (toVu perKg perM2))

        (n, gstdsr.Freq.Time |> parseTimeString) ,
        (gstdsr.Indication,
         fr,
         gstdsr.Norm   |> minmax false false,
         gstdsr.Abs    |> minmax false false,
         gstdsr.NormKg |> minmax true false,
         gstdsr.AbsKg  |> minmax true false,
         gstdsr.NormM2 |> minmax false true,
         gstdsr.AbsM2  |> minmax false true)



    let getSubstanceDoses (drs : DR.DoseRule seq) =
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
        |> Seq.groupBy fst
        |> Seq.map (fun (k, v) -> 
            k , 
            v 
            |> Seq.fold (fun acc (_, (ind, fr, norm, abs, normKg, absKg, normM2, absM2)) -> 
                let inds, frs, norm_, abs_, normKg_, absKg_, normM2_, absM2_ = acc

                let frs =
                    if frs |> List.exists ((=) fr) then frs else frs @ [ fr ]

                let inds =
                    if inds |> List.exists ((=) ind) then inds else inds @ [ ind ]

                let norm = [ norm; norm_ ] |> MinMax.foldMaximize
                let abs = [ abs; abs_ ] |> MinMax.foldMaximize
                let normKg = [ normKg; normKg_ ] |> MinMax.foldMaximize
                let absKg = [ absKg; absKg_ ] |> MinMax.foldMaximize
                let normM2 = [ normM2; normM2_ ] |> MinMax.foldMaximize
                let absM2 = [ absM2; absM2_ ] |> MinMax.foldMaximize

                inds, frs, norm, abs, normKg, absKg, normM2, absM2
            ) ([], [], MinMax.empty, MinMax.empty, MinMax.empty, MinMax.empty, MinMax.empty, MinMax.empty)
        )
        |> Seq.map (fun (k, (inds, frs, norm, abs, normKg, absKg, normM2, absM2)) ->
            k ,
            inds ,
            frs ,
            DoseRule.DoseRange.create 
                norm 
                (normKg, ValueUnit.Units.Weight.kiloGram) 
                (normM2, ValueUnit.Units.BSA.M2)
                abs 
                (absKg, ValueUnit.Units.Weight.kiloGram) 
                (absM2, ValueUnit.Units.BSA.M2)
        )
        |> Seq.map (fun ((n, time), inds, frs, dr)  ->
            let tu = 
                match frs with
                | fr::_ -> fr |> ValueUnit.get |> snd
                | _ -> ValueUnit.NoUnit

            inds ,
            DoseRule.Dosage.empty
            |> (Optic.set  Dosage.Name_ n)  
            |> (fun ds ->
                match time with
                | _ when time |> String.isNullOrWhiteSpace -> 
                    ds 
                    |> (Optic.set Dosage.SingleDosage_ dr) 
                | _  ->
                    ds
                    |> (Optic.set Dosage.TotalDosage_ (dr, tu))
            )
        ) 


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
        
        drs
        |> Seq.map (fun dr ->
            (dr.Indication ,
             Patient.empty
             |> mapAge dr.Age
             |> mapWght dr.Weight) , dr
        )
        |> Seq.groupBy fst
        |> Seq.map (fun (k, v) -> k |> snd, v |> Seq.map snd)
        |> Seq.map (fun (pat, drs) ->
            (pat, drs |> getSubstanceDoses)
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
    let getATCGroups (gpp: GenPresProduct) =
        ATC.get ()
        |> Array.filter (fun g -> 
            gpp
            |> getATCs
            |> Array.exists (fun a -> 
                a |> String.equalsCapInsens g.ATC5) && g.Shape = gpp.Shape
            )
        |> Array.distinct


    // Get the doserules for a genpresproduct
    // ToDo Temp hack ignore route and shape
    let getDoseRules (gpp : GenPresProduct) =
        gpp.Route
        |> Seq.collect (fun r ->
            RF.createFilter None None None None gpp.Name gpp.Shape r
            |> RF.find
            |> Seq.map (fun dr -> dr.Indication, (r, dr))
        )
        |> Seq.groupBy fst


   
    let createDoseRules n =
        GPP.filter n "" ""
        |> Seq.collect (fun gpp ->
            gpp 
            |> getATCGroups
            |> Seq.map (fun atc -> 
                (atc.Generic, atc.ATC5, atc.TherapeuticMainGroup, atc.TherapeuticSubGroup, atc.PharmacologicalGroup, atc.Substance) ,
                gpp
            )
        )
        |> Seq.groupBy fst
        |> Seq.sortBy fst
        |> Seq.map (fun (k, v) ->
            let gen, atc, tg, tsg, pg, sg = k
        
            DoseRule.create gen atc tg tsg pg sg [] ,
            v
            |> Seq.map snd
        )
        |> Seq.map (fun (dr, gpps) ->
            dr ,
            gpps
            |> Seq.collect (fun gpp ->
                gpp.Route
                |> Seq.collect (fun r ->
                    RF.createFilter None None None None gpp.Name gpp.Shape r
                    |> RF.find
                    |> getPatients
                    |> Seq.collect (fun (pat, sds) ->
                        sds
                        |> Seq.map (fun (ind, sds) -> ind, r, gpp.Shape, pat, sds)
                    )
                )
            )
            |> Seq.groupBy (fun (inds, _, _, _, _)  -> inds)
            |> Seq.sortBy fst
            |> Seq.map (fun (k, v) -> 
                k, 
                v
                |> Seq.map (fun (_, r, shp, pat, sds) -> r, shp, pat, sds)
                |> Seq.groupBy (fun (r, _, _, _)  -> r)
                |> Seq.sortBy fst
                |> Seq.map (fun (k, v) -> 
                    k, 
                    v
                    |> Seq.map (fun (_, shp, pat, sds) -> shp, pat, sds)
                    |> Seq.groupBy (fun (shp, _, _)  -> shp)
                    |> Seq.sortBy fst
                    |> Seq.map (fun (k, v) -> 
                        k, 
                        v
                        |> Seq.map (fun (_, pat, sds) -> pat, sds)
                        |> Seq.groupBy fst
                        |> Seq.sortBy fst
                        |> Seq.map (fun (k, v) -> k, v |> Seq.map snd )
                    )
                )
            )
        )
        |> Seq.map (fun (dr, inds) ->
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
                        let s, pats = shp

                        let dr =
                            acc
                            |> DoseRule.Optics.addShape ind r [s]
                    
                        pats
                        |> Seq.fold (fun acc pat ->
                            let pat, sds = pat

                            acc
                            |> DoseRule.Optics.addPatient ind r [s] pat
                            |> DoseRule.Optics.setSubstanceDosages ind r [s] pat (sds |> Seq.toList)
                        ) dr
                    ) dr
                ) dr
            ) dr
        )
