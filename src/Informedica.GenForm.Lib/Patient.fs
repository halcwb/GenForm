namespace Informedica.GenForm.Lib



module Patient = 

    open Informedica.GenUtils.Lib
    open Informedica.GenUtils.Lib.BCL
    open Informedica.GenForm.Lib.Utils

    open Aether
    open Aether.Operators

    type MinMax = MinMax.MinMax


    type Patient =
        {
            GestAge : MinMax
            Age : MinMax
            Weight : MinMax
            BSA : MinMax
            Gender : Gender
        }
    and Gender = Male | Female | Undetermined


    let create ga age wght bsa gend =
        {
            GestAge = ga
            Age = age
            Weight = wght
            BSA = bsa
            Gender = gend
        }


    let empty = create MinMax.empty MinMax.empty MinMax.empty MinMax.empty Undetermined


    type Patient with

        static member GestAge_ : 
            (Patient -> MinMax) * (MinMax -> Patient -> Patient) =
            (fun p -> p.GestAge), (fun a p -> { p with GestAge = a })

        static member Age_ : 
            (Patient -> MinMax) * (MinMax -> Patient -> Patient) =
            (fun p -> p.Age), (fun a p -> { p with Age = a })

        static member Weight_ : 
            (Patient -> MinMax) * (MinMax -> Patient -> Patient) =
            (fun p -> p.Weight), (fun w p -> { p with Weight = w })

        static member BSA_ : 
            (Patient -> MinMax) * (MinMax -> Patient -> Patient) =
            (fun p -> p.BSA), (fun b p -> { p with BSA = b })

        static member Gender_ : 
            (Patient -> Gender) * (Gender -> Patient -> Patient) =
            (fun p -> p.Gender), (fun g p -> { p with Gender = g })



    module Optics =
    
        module MinMax = MinMax.Optics


        let inclMinGestAge =
            Patient.GestAge_ >-> MinMax.inclMinLens


        let setInclMinGestAge = Optic.set inclMinGestAge


        let exclMinGestAge =
            Patient.GestAge_ >-> MinMax.exclMinLens


        let setExclMinGestAge = Optic.set exclMinGestAge


        let inclMaxGestAge =
            Patient.GestAge_ >-> MinMax.inclMaxLens


        let setInclMaxGestAge = Optic.set inclMaxGestAge


        let exclMaxGestAge =
            Patient.GestAge_ >-> MinMax.exclMaxLens


        let setExclMaxGestAge = Optic.set exclMaxGestAge


        let inclMinAge =
            Patient.Age_ >-> MinMax.inclMinLens


        let setInclMinAge = Optic.set inclMinAge


        let exclMinAge =
            Patient.Age_ >-> MinMax.exclMinLens


        let setExclMinAge = Optic.set exclMinAge


        let inclMaxAge =
            Patient.Age_ >-> MinMax.inclMaxLens


        let setInclMaxAge = Optic.set inclMaxAge


        let exclMaxAge =
            Patient.Age_ >-> MinMax.exclMaxLens


        let setExclMaxAge = Optic.set exclMaxAge


        let inclMinWeight =
            Patient.Weight_ >-> MinMax.inclMinLens


        let setInclMinWeight = Optic.set inclMinWeight


        let exclMinWeight =
            Patient.Weight_ >-> MinMax.exclMinLens


        let setExclMinWeight = Optic.set exclMinWeight


        let inclMaxWeight =
            Patient.Weight_ >-> MinMax.inclMaxLens


        let setInclMaxWeight = Optic.set inclMaxWeight


        let exclMaxWeight =
            Patient.Weight_ >-> MinMax.exclMaxLens


        let setExclMaxWeight = Optic.set exclMaxWeight


        let inclMinBSA =
            Patient.BSA_ >-> MinMax.inclMinLens


        let setInclMinBSA = Optic.set inclMinBSA


        let exclMinBSA =
            Patient.BSA_ >-> MinMax.exclMinLens


        let setExclMinBSA = Optic.set exclMinBSA


        let inclMaxBSA =
            Patient.BSA_ >-> MinMax.inclMaxLens


        let setInclMaxBSA = Optic.set inclMaxBSA


        let exclMaxBSA =
            Patient.BSA_ >-> MinMax.exclMaxLens


        let setExclMaxBSA = Optic.set exclMaxBSA


    let genderToString = function
    | Male -> "man"
    | Female -> "vrouw"
    | Undetermined -> ""


    let toString ({ GestAge = ga; Age = age; Weight = wght; BSA = bsa; Gender = gen }) =
        let (>+) sl sr = 
            let l, s = sr

            let s = s |> String.trim
            let sl = sl |> String.trim
            
            if s |> String.isNullOrWhiteSpace then sl
            else sl + (if sl = "" then " " else  ", ") + l + s
        
        ""
        >+ ("Zwangerschapsduur: ", ga |> MinMax.gestAgeToString)
        >+ ("Leeftijd: ", age |> MinMax.ageToString)
        >+ ("Gewicht: ", wght |> MinMax.toString)
        >+ ("BSA: ", bsa |> MinMax.toString)
        >+ ("Geslacht: ", gen |> genderToString)
        |> String.removeTrailingEOL
