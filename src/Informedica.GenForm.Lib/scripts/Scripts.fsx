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
    
let remBr s = 
    (String.regex "\[[^\]]*]").Replace(s, "")


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
|> remBr


open Informedica.GenForm.Lib.DoseRule


let mapMinMax setMin setMax (minmax : DoseRule.MinMax) dr =
    dr
    |> setMin minmax.Min
    |> setMax minmax.Max


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

    let addSubstances (dsrs : Informedica.GenProduct.Lib.DoseRule.DoseRule []) (dr : DoseRule) =
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
                    state
                    |> addSubstance (Substance.init n u)
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
                            let times = 
                                pat
                                |> snd
                                |> Array.groupBy (fun dr ->
                                    dr.Freq.Time
                                )
                            // Group per frequency time
                            for time in times do
                                let tm, drs = time
                                let (gen, age, wght, bsa) , _ = pat

                                yield empty
                                      |> setGeneric gpp.Name
                                      |> setATC atc
                                      |> setTherapyGroup g.TherapeuticMainGroup
                                      |> setTherapySubGroup g.TherapeuticSubGroup
                                      |> setShapeName gpp.Shape
                                      |> setRouteName r
                                      |> (fun dr ->
                                            if gen = "man" then dr |> setPatientMaleGender true
                                            elif gen = "vrouw" then dr |> setPatientFemaleGender true
                                            else dr
                                      )
                                      |> mapMinMax setPatientMinAge setPatientMaxAge age
                                      |> mapMinMax setPatientMinWeight setPatientMaxWeight wght
                                      |> mapMinMax setPatientMinBSA setPatientMaxBSA bsa
                                      |> addSubstances drs
                                      |> (fun dr ->
                                        { dr with Text = drs |> getText }
                                      )
                                      

    ]
        


mapDoseRule (GenPresProduct.filter "diclofenac" "" "")
|> List.map (fun dr -> dr |> toString)
|> List.iter (printfn "%s")



DoseRule.get ()
|> Array.map (fun dr ->
    dr.Freq.Time
)
|> Array.distinct
|> Array.sort



DoseRule.get ()
|> Array.map (fun dr ->
    dr.Freq
)
|> Array.distinct
|> Array.map mapFreq
|> Array.distinct
|> Array.sort
|> Array.iter (printfn "%A")

BigRational.Parse("1")
1N |> BigRational.toString
