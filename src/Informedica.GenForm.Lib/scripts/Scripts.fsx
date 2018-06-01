#load "references.fsx"

#time

open System

let pwd = Environment.GetEnvironmentVariable("HOME")
Environment.CurrentDirectory <- pwd + "/Development/GenForm/" //__SOURCE_DIRECTORY__ + "/../../../"

open MathNet.Numerics

open Informedica.GenUtils.Lib
open Informedica.GenUtils.Lib.BCL
open Informedica.GenUnits.Lib
open Informedica.GenUnits.Lib.Api
open Informedica.GenProduct.Lib
open Informedica.GenForm.Lib

FilePath.formulary |> (fun p -> printfn "%s" p; p) |> File.exists
let printResult m r = printf m; printfn " %A" r; r
    

DoseRule.empty
|> DoseRule.setGeneric "pacetamol"
|> DoseRule.setShapeName "zetpil"
|> DoseRule.setRouteName "RECTAAL"
|> DoseRule.setATC "AB2029"
|> DoseRule.setTherapyGroup "Pijnstilling"
|> DoseRule.setTherapySubGroup "Pijnstilling paracetamol"
|> DoseRule.setIndication "pijn"
|> DoseRule.setPatientMinAge (Some 10.)
|> DoseRule.setPatientMaxAge (None)
|> DoseRule.setPatientFemaleGender true
|> DoseRule.setPatientMinWeight (Some 5.)
|> DoseRule.setPatientMaxWeight (Some 100.)
|> DoseRule.setPatientMinBSA (Some 2.0)
|> DoseRule.setPatientMaxBSA (Some 1.0)
|> DoseRule.toString


open Informedica.GenForm.Lib.DoseRule
open Informedica.GenForm.Lib.DoseRule.Dose


let mapMinMax setMin setMax (minmax : DoseRule.MinMax) dr =
    dr
    |> setMin minmax.Min
    |> setMax minmax.Max


let mapTime s =
    s
    |> String.replace "per " ""
    |> String.replace "dagen" "dag"
    |> String.replace "weken" "week"
    |> String.replace "maanden" "maand"
    |> String.replace "minuten" "minuut"
    |> String.replace "uren" "uur"
    |> String.replace "eenmalig" ""
    |> (fun s -> 
        if s |> String.isNullOrWhiteSpace then "1 X[Count]"
        else s + "[Time]"
    )
    |> (fun s' -> 
        match s' |> String.split " " with
        | [v;u] -> s + v + " " + u
        | [u]   -> s + "1" + " " + u
        | _ -> ""
    )
    |> ValueUnit.Units.fromString


let mapFreq (fr: DoseRule.Frequency) =
    let s = fr.Frequency |> string
    let s = s + " X[Count]/"
    fr.Time
    |> String.replace "per " ""
    |> String.replace "dagen" "dag"
    |> String.replace "weken" "week"
    |> String.replace "maanden" "maand"
    |> String.replace "minuten" "minuut"
    |> String.replace "uren" "uur"
    |> String.replace "eenmalig" ""
    |> (fun s -> 
        if s |> String.isNullOrWhiteSpace then "1 X[Count]"
        else s + "[Time]"
    )
    |> (fun s' -> 
        match s' |> String.split " " with
        | [v;u] -> s + v + " " + u
        | [u]   -> s + "1" + " " + u
        | _ -> ""
    )
    |> ValueUnit.fromString


let mapDoses qty u (dr : Informedica.GenProduct.Lib.DoseRule.DoseRule) =
    let fr = mapFreq dr.Freq

    let toVu perKg perM2 v =
        let u = 
            if not perKg then u
            else
                u |> ValueUnit.per ValueUnit.Units.Weight.kiloGram

        let u = 
            if not perM2 then u
            else
                u |> ValueUnit.per ValueUnit.Units.BSA.M2

        match u |> ValueUnit.fromFloat (v * qty) with
        | Some vu -> vu * fr |> Some
        | None -> None
    
    let minmax perKg perM2 (mm : Informedica.GenProduct.Lib.DoseRule.MinMax) =
        MinMax.empty
        |> MinMax.setMinOpt (mm.Min |> Option.bind (toVu perKg perM2))
        |> MinMax.setMaxOpt (mm.Max |> Option.bind (toVu perKg perM2))

    dr.Norm   |> minmax false false,
    dr.Abs    |> minmax false false,
    dr.NormKg |> minmax true false,
    dr.AbsKg  |> minmax true false,
    dr.NormM2 |> minmax false true,
    dr.AbsM2  |> minmax false true

let mapDoseRule (gpps : GenPresProduct.GenPresProduct []) =

    let getDrs (gpp :GenPresProduct.GenPresProduct) rt =
        RuleFinder.createFilter None None None None gpp.Name gpp.Shape rt
        |> RuleFinder.find

    let getText drs =
        drs
        |> Array.map (fun dr -> dr |> DoseRule.toString2)
        |> Array.distinct
        |> String.concat "\n" 
        |> GSTandText

    let atcs (gpp : GenPresProduct.GenPresProduct) =
        gpp.GenericProducts
        |> Array.map(fun gp -> gp.ATC)
        |> Array.distinct

    let rts (gpp: GenPresProduct.GenPresProduct) = 
        gpp.GenericProducts
        |> Array.collect (fun gp -> gp.Route)
        |> Array.distinct

    let tgs (gpp: GenPresProduct.GenPresProduct) =
        ATCGroup.get ()
        |> Array.filter (fun g -> 
            atcs gpp |> Array.exists (fun a -> a |> String.equalsCapInsens g.ATC5) &&
            g.Shape = gpp.Shape
        )

    let addSubstances period (dsrs : Informedica.GenProduct.Lib.DoseRule.DoseRule []) (dr : DoseRule) =
        let period = period |> mapTime

        let substs = 
            dsrs
            |> Array.collect (fun r ->
                r.GenericProduct
                |> Array.collect (fun gp -> 
                    gp.Substances 
                    |> Array.map (fun s -> 
                        let u = s.Unit
                        s.Name, u
                    )
                )                
            )
            |> Array.distinct
            |> Array.map (fun (n, u) ->
                let qts =
                    dsrs
                    |> Array.collect (fun r -> 
                        r.GenericProduct
                        |> Array.collect (fun gp ->
                            gp.Substances
                            |> Array.filter (fun s -> s.Name = n)
                            |> Array.map (fun s -> r, s.Quantity)
                        )
                    )
                n, u, qts
            )
            
        dr
        |> (fun dr -> 
            substs
            |> Array.fold (fun state (n, u, qts) ->
                match u |> ValueUnit.createFromGStand 1. with
                | Some vu ->
                    let _, u = vu |> ValueUnit.get

                    let doses =
                        qts
                        |> Array.map(fun (r, v) ->
                            mapDoses v u r
                        )

                    let norm = 
                        doses
                        |> Array.map(fun (norm, _, _, _, _, _) -> norm)
                        |> Array.toList
                        |> MinMax.foldMaximize
                       

                    let abs = 
                        doses
                        |> Array.map(fun (_, abs, _, _, _, _) -> abs)
                        |> Array.toList
                        |> MinMax.foldMaximize

                    let normKg = 
                        doses
                        |> Array.map(fun (_, _, normKg, _, _, _) -> normKg)
                        |> Array.toList
                        |> MinMax.foldMaximize

                    let absKg = 
                        doses
                        |> Array.map(fun (_, _, _, absKg, _, _) -> absKg)
                        |> Array.toList
                        |> MinMax.foldMaximize

                    let normM2 = 
                        doses
                        |> Array.map(fun (_, _, _, _, normM2, _) -> normM2)
                        |> Array.toList
                        |> MinMax.foldMaximize

                    let absM2 = 
                        doses
                        |> Array.map(fun (_, _, _, _, _, absM2) -> absM2)
                        |> Array.toList
                        |> MinMax.foldMaximize

                    let dose =
                        Dose.emptyDoseValue
                        |> Dose.setNormDose norm
                        |> Dose.setAbsDose abs
                        |> Dose.setNormDosePerKg normKg
                        |> Dose.setAbsDosePerKg absKg
                        |> Dose.setNormDosePerM2 normM2
                        |> Dose.setAbsDosePerM2 absM2

                    let frqs =
                        qts
                        |> Array.map (fun (r, _) ->
                            r.Freq.Frequency
                            |> BigRational.fromFloat
                        )
                        |> Array.distinct
                        |> Array.filter Option.isSome
                        |> Array.map Option.get
                        |> Array.sort
                        |> Array.toList

                    state
                    |> addSubstance (
                        Substance.init n u
                        |> Substance.setDose (Dose.empty |> Dose.add frqs period dose)
                    )
                | None -> state
            ) dr
        )

    // Map GStand DoseRule to DoseRule
    [
        for gpp in gpps do
            // Group per route
            for r in gpp |> rts do
                let drs = getDrs gpp r
                // Get ATC's
                for atc in gpp |> atcs do
                    // Get ATC Group's 
                    for g in gpp |> tgs do
                        let pats =
                            drs
                            |> Array.groupBy (fun dr ->
                                dr.Gender, dr.Age, dr.Weight, dr.BSA
                            )
                        // Group per patient
                        for pat in pats do
                            let inds =
                                let _, rs = pat
                                rs
                                |> Array.groupBy (fun r ->
                                    r.Indication
                                )
                            for ind in inds do
                                
                                let times = 
                                    ind
                                    |> snd
                                    |> Array.groupBy (fun dr ->
                                        dr.Freq.Time
                                    )
                                // Group per frequency time
                                for time in times do
                                    let ind = ind |> fst
                                    let tm, drs = time
                                    let (gen, age, wght, bsa) , _ = pat

                                    yield DoseRule.empty
                                          |> setGeneric gpp.Name
                                          |> setATC atc
                                          |> setTherapyGroup g.TherapeuticMainGroup
                                          |> setTherapySubGroup g.TherapeuticSubGroup
                                          |> setShapeName gpp.Shape
                                          |> setRouteName r
                                          |> setIndication ind
                                         
                                          |> (fun dr ->
                                                if gen = "man" then dr |> setPatientMaleGender true
                                                elif gen = "vrouw" then dr |> setPatientFemaleGender true
                                                else dr
                                          )

                                          |> mapMinMax setPatientMinAge setPatientMaxAge age
                                          |> mapMinMax setPatientMinWeight setPatientMaxWeight wght
                                          |> mapMinMax setPatientMinBSA setPatientMaxBSA bsa
                                          |> addSubstances tm drs
                                          |> (fun dr ->
                                            { dr with Text = drs |> getText }
                                          )
                                      

    ]
        


mapDoseRule (GenPresProduct.filter "diclofenac" "" "")
|> List.map (fun dr -> dr |> DoseRule.toString)
|> List.iter (printfn "%s")

