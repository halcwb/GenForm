﻿namespace Informedica.GenForm.Lib
    
module Patient =

    open MathNet.Numerics

    open Informedica.GenUtils.Lib
    open Informedica.GenUtils.Lib.BCL

    open Informedica.GenUnits.Lib

    open ValueUnit


    type Age =
        | BirthDate of (BirthYear * BirthMonth * BirthDay)
        | AgeMonths of float
        | NoAge
    and BirthYear = int
    and BirthMonth = int
    and BirthDay = int


    type Weight = ValueUnit Option


    type Height = ValueUnit Option


    type Gender = Male | Female | Undetermined


    let genderToString = function
        | Male -> "male"
        | Female -> "Female"
        | Undetermined -> "Undetermined"


    type GestationalAge = 
        | GestAge of GestWeeks * GestDays
        | NoGestAge
    and GestWeeks = int
    and GestDays = int



    module Mapping =

        let weightMap =
            [
                Units.Weight.kiloGram, [ "kg"; "kilogram"]
                Units.Weight.gram, [ "g"; "gr"; "gram" ]
            ]

        let heightMap =
            [
                Units.Height.meter, [ "m"; "meter"]
                Units.Height.centiMeter, [ "cm"; "centimeter" ]
            ]

        let genderMap =
            [
                Male, ["man"; "m"; "male"]
                Female, ["vrouw"; "v"; "vr"; "female"]
            ]

        let map mapping s =
            mapping |> List.tryFind (fun (w, xs) ->
                xs |> List.exists (String.equalsCapInsens s)
            ) 
            |> Option.bind (fst >> Some)


        let mapWeight = map weightMap


        let mapHeight = map heightMap


        let mapGender s = 
            match s |> map genderMap with
            | Some g -> g
            | None -> Undetermined



    type Patient =
        {
            Age : Age
            Gest : GestationalAge
            Weight : Weight
            Height : Height
            Gender : Gender
        }


    let empty = 
        {
            Age = NoAge
            Gest = NoGestAge
            Weight = None
            Height = None
            Gender = Undetermined
        }


    let create by bm bd am gw gd wt wu ln lu gn =
        let age = 
            match by, bm, bd with
            | Some y, Some m, Some d -> (y, m, d) |> BirthDate
            | _ -> 
                match am with
                | Some a -> a |> AgeMonths
                | None -> NoAge
        
        let gest =
            match gw, gd with
            | Some w, Some d -> (w, d) |> GestAge
            | _ -> NoGestAge

        let weight = 
            let u = wu |> Mapping.mapWeight

            match u with
            | Some un ->
                wt 
                |> BigRational.fromFloat
                |> Option.bind (fun br -> br |> create un |> Some)
            | None -> None
          
        let length = 
            let u = lu |> Mapping.mapHeight

            match u with
            | Some un ->
                ln 
                |> BigRational.fromFloat
                |> Option.bind (fun br -> br |> create un |> Some)
            | None -> None
          
        let gender = Mapping.mapGender gn

        {
            Age    = age
            Gest   = gest
            Weight = weight
            Height = length
            Gender = gender
        }


    let apply f (pat : Patient) = f pat


    let get = apply id


    let getWeight pat =
        (pat |> get).Weight


    let getHeight pat =
        (pat |> get).Height


    /// Calculate the BSA according to the formula:
    /// bsa = (weight (kg) ** 0.425) * (height (cm) ** 0.725) * 0.007184
    let calculateBSA (pat : Patient) =
        match pat |> getWeight, pat |> getHeight with
        | Some w, Some h ->
            let n1 = 0.425    
            let n2 = 0.725    
            let n3 = 0.007184 

            let w = 
                w 
                ==> Units.Weight.kiloGram
                |> ValueUnit.get 
                |> fst
                |> BigRational.toFloat

            let h = 
                h 
                ==> Units.Height.centiMeter
                |> ValueUnit.get 
                |> fst
                |> BigRational.toFloat

            (w ** n1) * (h ** n2) * n3 
            |> BigRational.fromFloat

        | _ -> None
        