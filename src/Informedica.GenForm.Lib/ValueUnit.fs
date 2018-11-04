﻿namespace Informedica.GenForm.Lib



module ValueUnit =

    open MathNet.Numerics

    open Informedica.GenForm.Lib.Utils

    open Informedica.GenUtils.Lib
    open Informedica.GenUtils.Lib.BCL
    open Informedica.GenUnits.Lib

    open ValueUnit


    let getValue vu = let v, _ = vu |> ValueUnit.get in v


    let unitToString u =
        u
        |> Units.toString Units.Dutch Units.Short
        |> String.remBr


    let unitFromString m u = 
            u
            |> Mapping.mapUnit m Mapping.ValueUnitMap
            |> Units.fromString


    let unitFromGStandString = unitFromString Mapping.GStandMap


    let unitFromAppString = unitFromString Mapping.AppMap


    let createUnit m v u =
        let s = 
            u
            |> Mapping.mapUnit m Mapping.ValueUnitMap
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


    let createFromFormul = createUnit Mapping.AppMap


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
        |> Mapping.mapUnit Mapping.ValueUnitMap Mapping.GStandMap


    let timeMinute = (fun n -> fromFloat n Units.Time.minute)


    let timeHour =  (fun n -> fromFloat n Units.Time.hour)


    let timeDay =  (fun n -> fromFloat n Units.Time.day)


    let timeWeek =  (fun n -> fromFloat n Units.Time.week)


    let ageInWk =  (fun n -> fromFloat n Units.Time.week)


    let ageInMo =  (fun n -> fromFloat n Units.Time.month)


    let ageInYr =  (fun n -> fromFloat n Units.Time.year)


    let weightInKg =  (fun n -> fromFloat n Units.Weight.kiloGram)


    let bsaInM2 =  (fun n -> fromFloat n Units.BSA.M2)


    let freqUnitPerNday n = 
        1N 
        |> Units.Count.nTimes
        |> per (Units.Time.nDay n)


    let freqUnitPerNHour n = 
        1N 
        |> Units.Count.nTimes
        |> per (Units.Time.nHour n)


    let freqPerOneHour = freqUnitPerNHour 1N


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


    let isTimeUnit u =
        (u |> ValueUnit.Group.unitToGroup) = ValueUnit.Group.TimeGroup
        

    module Units =

        let perOneHour = freqPerOneHour

        let perFourHour = freqUnitPerNHour 4N

        let perOneDay = freqUnitPerNday 1N
        
        let mgKgDay = Units.Mass.milliGram   |> per Units.Weight.kiloGram |> per Units.Time.day

        let mgKgHour = Units.Mass.milliGram  |> per Units.Weight.kiloGram |> per Units.Time.hour

        let mgKg4Hour = Units.Mass.milliGram |> per Units.Weight.kiloGram |> per (Units.Time.nHour 4N)

        let mcgKgHour = Units.Mass.microGram |> per Units.Weight.kiloGram |> per Units.Time.hour

        let mcgKgMin = Units.Mass.microGram  |> per Units.Weight.kiloGram |> per Units.Time.minute
            
        let mcgKgDay = Units.Mass.microGram |> per Units.Weight.kiloGram |> per Units.Time.day
