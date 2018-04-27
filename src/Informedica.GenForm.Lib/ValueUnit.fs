namespace Informedica.GenForm.Lib


module ValueUnit =

    open MathNet.Numerics

    open Informedica.GenUtils.Lib.BCL
    open Informedica.GenUnits.Lib

    module Units =


        module Time = 

            let year = CombiUnit.Units.timeYear 1N

            let month = CombiUnit.Units.timeMonth 1N

            let week = CombiUnit.Units.timeWeek 1N

            let day = CombiUnit.Units.timeDay 1N

            let hour = CombiUnit.Units.timeHour 1N

            let minute = CombiUnit.Units.timeMinute 1N


        module Patient =

            let kg = CombiUnit.Units.weightKg 1N

            let gr = CombiUnit.Units.weightGram 1N

            let bsa = CombiUnit.Units.bsaM2 1N


        module Substance =

            let gram = CombiUnit.Units.massGram

            let mg = CombiUnit.Units.massMilliGram

            let mcg = CombiUnit.Units.massMicroGram

            let ng = CombiUnit.Units.massNanoGram


        module Volume =
    
            let l = CombiUnit.Units.volumeLiter

            let dl = CombiUnit.Units.volumeDeciLiter

            let ml = CombiUnit.Units.volumeMilliLiter

            let mcl = CombiUnit.Units.volumeMicroLiter


    let createUnit m v u =
        let s = 
            u
            |> Mapping.mapUnit m Mapping.GenFormMap
        if s = "" then None
        else
            v
            |> BigRational.fromFloat 
            |> Option.bind (fun v ->
                s
                |> CombiUnit.fromString
                |> ValueUnit.create v
                |> Some
            )


    let createFromGStand = createUnit Mapping.GStandMap


    let createFromFormul = createUnit Mapping.FormMap


    let fromFloat v u =
        v
        |> BigRational.fromFloat
        |> Option.bind (fun br ->
            ValueUnit.create br u
            |> Some
        )


    let substanceInGStandUnit v u =
        let substanceUnit u =
            let s = 
                u 
                |> Mapping.mapUnit Mapping.GStandMap Mapping.GenFormMap

            if s = "" then None
            else 
                s 
                |> CombiUnit.fromString
                |> Some

        match u |> substanceUnit with
        | Some u' -> fromFloat v u'
        | None -> None


    let getSubstanceInGStandUnit vu =
        let v, u = ValueUnit.get vu

        v |> BigRational.toFloat, 
        u 
        |> CombiUnit.toString
        |> Mapping.mapUnit Mapping.GenFormMap Mapping.GStandMap


    let ageInMo = Option.bind (fun n -> fromFloat n Units.Time.month)


    let weightInKg = Option.bind (fun n -> fromFloat n Units.Patient.kg)


    let bsaInM2 = Option.bind (fun n -> fromFloat n Units.Patient.bsa)


    let gestAgeInDaysAndWeeks gest =
        gest 
        |> Option.bind (fun (w, d) ->
            fromFloat w Units.Time.week
            |> Option.bind (fun vu1 -> 
                fromFloat d Units.Time.day
                |> Option.bind (fun vu2 -> vu1 + vu2 |> Some)
            )
        )

