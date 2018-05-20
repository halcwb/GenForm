namespace Informedica.GenForm.Lib

module ValueUnit =


    open Informedica.GenUtils.Lib
    open Informedica.GenUtils.Lib.BCL
    open Informedica.GenUnits.Lib

    open ValueUnit


    let createUnit m v u =
        let s = 
            u
            |> Mapping.mapUnit m Mapping.GenFormMap
        if s = "" then None
        else
            v
            |> BigRational.fromFloat 
            |> Option.bind (fun v ->
                (v |> BigRational.toString) + " " + s
                |> fromString
                |> Some
            )


    let createFromGStand = createUnit Mapping.GStandMap


    let createFromFormul = createUnit Mapping.FormMap


    let fromFloat v u =
        v
        |> BigRational.fromFloat
        |> Option.bind (fun br ->
            ValueUnit.create u br
            |> Some
        )


    let substanceInGStandUnit = createFromGStand
 

    let getSubstanceInGStandUnit vu =
        let v, u = ValueUnit.get vu

        v |> BigRational.toFloat, 
        u 
        |> Units.toString
        |> Mapping.mapUnit Mapping.GenFormMap Mapping.GStandMap


    let ageInMo = Option.bind (fun n -> fromFloat n Units.Time.month)


    let weightInKg = Option.bind (fun n -> fromFloat n Units.Weight.kiloGram)


    let bsaInM2 = Option.bind (fun n -> fromFloat n Units.BSA.M2)


    let gestAgeInDaysAndWeeks gest =
        gest 
        |> Option.bind (fun (w, d) ->
            fromFloat w Units.Time.week
            |> Option.bind (fun vu1 -> 
                fromFloat d Units.Time.day
                |> Option.bind (fun vu2 -> vu1 + vu2 |> Some)
            )
        )

    let toStringPrec prec vu = 
        let v, u = vu |> ValueUnit.get

        let vs = 
            v
            |> BigRational.toFloat
            |> Double.fixPrecision prec
            |> string

        let us = u |> ValueUnit.Units.toString

        vs + " " + us
