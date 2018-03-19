﻿namespace Informedica.GenProduct.Lib

module ProductRange =

    open System
    open Informedica.GenUtils.Lib.BCL
    open Informedica.GenUtils.Lib

    [<Literal>]
    let productFile = @"../../resource/productrange/AssortimentDB.csv"

    // GPK;ATC;HoofdGroep;SubGroep;Generiek;Product;Etiket;Vorm;Route;Sterkte;Eenheid;StandDose;DoseEenheid;Indicaties;InFormDb
    type ProductRange =
        {
            GPK: int Option
            ATC: string
            MainGroup: string
            SubGroup: string
            Generic: string
            Product: string
            Label: string
            Shape: string
            Route: string
            Concentration: float Option
            ConcentrationUnit : string
            DoseMultiple: float Option
            DoseMultipleUnit: string
            Indications: string
        }

    let create gpk atc grp sub gen prod lbl shp route conc concU dose doseU inds =
        {
            GPK = gpk // 0
            ATC = atc
            MainGroup = grp
            SubGroup = sub
            Generic = gen
            Product = prod
            Label = lbl
            Shape = shp
            Route = route
            Concentration = conc // 9
            ConcentrationUnit = concU
            DoseMultiple = dose // 11
            DoseMultipleUnit = doseU
            Indications = inds
        }

    let data _ = 
        File.readAllLines productFile
        |> Array.skip 1
        |> Array.map (String.splitAt ';')
        |> Array.map (fun sa -> 
//            printfn "%s" (sa |> Array.toString)
            create (sa.[0] |> Int32.tryParse)
                   sa.[1]
                   sa.[2]
                   sa.[3]
                   sa.[4]
                   sa.[5]
                   sa.[6]
                   sa.[7] 
                   sa.[8]
                   (sa.[9] |> Double.tryParse)
                   sa.[10]
                   (sa.[11] |> Double.tryParse)
                   sa.[12]
                   sa.[13]
        )
 