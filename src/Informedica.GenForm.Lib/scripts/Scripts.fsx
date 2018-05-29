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

let ml50 = 50N |> ValueUnit.create ValueUnit.Units.Volume.milliLiter
let l5 = 5N |> ValueUnit.create ValueUnit.Units.Volume.liter
ml50 < l5

open ValueUnit
open Informedica.GenForm.Lib.DoseRule

ml50 <? l5

    
// Testing

RuleFinder.createFilter None None None (Some 121967) "" "" "iv"
|> RuleFinder.find
|> RuleFinder.convertToResult
|> Option.bind (fun rs ->
    rs.Doses
    |> Array.groupBy (fun d ->
        d.Freq.Time
    ) |> Some
)

GenPresProduct.getAssortment ()
|> Array.collect (fun gpp ->
    gpp.GenericProducts
    |> Array.map (fun gp -> gp.Id)
)
|> Array.distinct

let test = 
    { Dto.dto with
        BirthYear = 2018
        BirthMonth = 4
        BirthDay = 10
        WeightKg = 1.6
        LengthCm = 50.
        GPK = "3689"
        MultipleUnit = "mg"
        Route = "iv"
    }

Dto.findRules test


"1 Times[Count]/1 Day[Time]"
|> ValueUnit.fromString

GenPresProduct.getAssortment ()
|> Array.collect (fun gpp ->
    gpp.Route
)
|> Array.distinct
|> Array.sort
|> Array.iter (printfn "%s")


let getProduct gen shp =
    let gpps =
        GenPresProduct.getAssortment ()
        |> Array.filter (fun gpp ->
            gpp.Name  |> String.equalsCapInsens gen &&
            gpp.Shape |> String.equalsCapInsens shp
        )
    if gpps |> Array.length <> 1 then None
    else
        let gpp = gpps.[0]
        {
            Product.empty with
                Name = gpp.Name 
                Shape = gpp.Shape
                Unit = gpp.Unit
                
        }
        |> Some

getProduct "paracetamol" "zetpil"

    
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
                        let qts =
                            gp.Substances
                            |> Array.filter (fun s' -> s'.Name = s.Name)
                            |> Array.map (fun s' -> s'.Quantity)
                            |> Array.distinct
                        s.Name, qts, u
                    )
                )                
            )
            |> Array.distinct
            
        dr
        |> (fun dr -> 
            substs
            |> Array.fold (fun state (n, qts, u) ->
                match u |> ValueUnit.createFromGStand 1. with
                | Some vu ->
                    let _, u = vu |> ValueUnit.get
                    state
                    |> addSubstance (Substance.init n u)
                | None -> state
            ) dr
        )

    // Map GenPresProduct to DoseRule
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


RuleFinder.createFilter None None None (Some 121967) "" "" ""
|> RuleFinder.find


RuleFinder.createFilter None None None None "diclofenac" "" ""
|> RuleFinder.find
|> Array.map (fun dr -> 
    (dr.Routes |> Array.map RuleFinder.createRoute, dr)
)
|> Array.groupBy (fun (r, _) ->
    r
)
|> Array.map (fun (r, drs) ->
    let drs =
        drs
        |> Array.map snd
        |> Array.groupBy (fun dr -> 
            (dr.Gender, dr.Age, dr.Weight, dr.BSA)
        )
    (r, drs)
)
|> Array.map (fun (r, drs) ->
    let drs = 
        drs |> Array.map (fun (pat, drs) ->
            (pat, drs |> Array.map (DoseRule.toString ", ") |> Array.distinct)
        )
    (r, drs)
)

