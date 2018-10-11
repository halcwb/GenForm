namespace Informedica.GenForm.Lib



module ValueUnit =

    open Informedica.GenForm.Lib.Utils

    open Informedica.GenUtils.Lib
    open Informedica.GenUtils.Lib.BCL
    open Informedica.GenUnits.Lib

    open ValueUnit


    let unitToString u =
        u
        |> Units.toString Units.Dutch Units.Short
        |> String.remBr


    let unitFromString m u = 
            u
            |> Mapping.mapUnit m Mapping.GenFormMap
            |> Units.fromString

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
        |> unitToString
        |> Mapping.mapUnit Mapping.GenFormMap Mapping.GStandMap


    let timeMinute = (fun n -> fromFloat n Units.Time.minute)


    let timeHour =  (fun n -> fromFloat n Units.Time.hour)


    let timeDay =  (fun n -> fromFloat n Units.Time.day)


    let timeWeek =  (fun n -> fromFloat n Units.Time.week)


    let ageInWk =  (fun n -> fromFloat n Units.Time.week)


    let ageInMo =  (fun n -> fromFloat n Units.Time.month)


    let ageInYr =  (fun n -> fromFloat n Units.Time.year)


    let weightInKg =  (fun n -> fromFloat n Units.Weight.kiloGram)


    let bsaInM2 =  (fun n -> fromFloat n Units.BSA.M2)


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

        let us = 
            u 
            |> unitToString

        vs + " " + us
