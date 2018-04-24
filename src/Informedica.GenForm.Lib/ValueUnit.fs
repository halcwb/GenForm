namespace Informedica.GenForm.Lib

module ValueUnit =

    open MathNet.Numerics

    open Informedica.GenUtils.Lib.BCL
    open Informedica.GenUnits.Lib


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
        let monthUnit = 
            Unit.Units.month
            |> CombiUnit.create 1N

        v
        |> Option.bind (fun n ->
            (n |> BigRational.fromFloat)
            |> Option.bind (fun br -> 
                ValueUnit.create br monthUnit
                |> (fun vu -> 
                    if br > 12N then
                        let yr = Unit.Units.year |> CombiUnit.create 1N
                        vu |> ValueUnit.convertTo yr
                    else vu
                )
                |> Some
            )
        )


    let weightInKg v = 
        let weightKg = 
            Unit.Units.weightKg
            |> CombiUnit.create 1N 

        v
        |> Option.bind (fun n -> fromFloat n weightKg)
