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
        | Some u' ->
            v
            |> Option.bind (fun n ->
                fromFloat n u'
            )
        | None -> None


    let ageInMo v =

        v
        |> Option.bind (fun n ->
            (n |> BigRational.fromFloat)
            |> Option.bind (fun br -> 
                ValueUnit.create br Units.Time.month
                |> (fun vu -> 
                    if br > 12N then
                        vu |> ValueUnit.convertTo Units.Time.year
                    else vu
                )
                |> Some
            )
        )


    let weightInKg v = 
        v
        |> Option.bind (fun n -> fromFloat n Units.Patient.kg)
