#load "references.fsx"

#time 

open MathNet.Numerics
open Giraffe

module Multipliers = Informedica.GenUnits.Lib.Unit.Multipliers

type Value = BigRational

type ValueUnit = Value * Unit
and Unit = 
    | NoUnit
    | Unit of UnitGroup
    | CombiUnit of Unit * Operator * Unit
and Operator =
    | OpTimes
    | OpPer
    | OpPlus
    | OpMinus
and UnitGroup =
    | Count of CountUnit
    | Mass of MassUnit
    | Volume of VolumeUnit
    | Time of TimeUnit
    | Molar of MolarUnit
    | InterNatUnit of IUnit
    | Weight of WeightUnit
    | Height of HeightUnit
    | BSA of BSAUnit
and CountUnit = 
    | Times of Times
and MassUnit = 
    | KiloGram of KiloGram
    | Gram of Gram
    | MilliGram of MilliGram
    | MicroGram of MicroGram
    | NanoGram of NanoGram
and VolumeUnit =
    | Liter of Liter
    | DeciLiter of DeciLiter
    | MilliLiter of MilliLiter
    | MicroLiter of MicroLiter
and TimeUnit =
    | Year of Year
    | Month of Month
    | Week of Week
    | Day of Day
    | Hour of Hour
    | Minute of Minute
    | Second of Second
and MolarUnit =
    | Mol of Mol
    | MilliMol of MilliMol
and IUnit =
    | MIU of MIU
    | IU of IU
and WeightUnit = 
    | WeightKiloGram of KiloGram
    | WeightGram of Gram
and HeightUnit = 
    | HeightMeter of Meter
    | HeightCentiMeter of CentiMeter
and BSAUnit = 
    | M2 of M2
// Count
and Times = BigRational
and IU = BigRational
// InterNatUnit
and MIU = BigRational
// Mass
and KiloGram    = BigRational
and Gram    = BigRational
and MilliGram   = BigRational
and MicroGram   = BigRational
and NanoGram    = BigRational
and Liter = BigRational
// Volume
and DeciLiter  = BigRational
and MilliLiter = BigRational
and MicroLiter = BigRational
// Time
and Second  = BigRational
and Minute  = BigRational
and Hour    = BigRational
and Day     = BigRational
and Week    = BigRational
and Month   = BigRational
and Year    = BigRational
// Height
and CentiMeter   = BigRational
and Meter    = BigRational
// Molar
and Mol  =  BigRational
and MilliMol =  BigRational
// BSA
and M2      = BigRational


let create v u : ValueUnit = (v, u)

1N |> WeightKiloGram |> Weight |> Unit |> create 10N


let getUnitGroupMultiplier = function 
    | Count g ->
        match g with
        | Times n -> n * n * Multipliers.one
    | Mass g  ->
        match g with
        | KiloGram n  -> n * Multipliers.kilo
        | Gram n      -> n * Multipliers.one
        | MilliGram n -> n * Multipliers.milli
        | MicroGram n -> n * Multipliers.micro
        | NanoGram n  -> n * Multipliers.nano
    | Volume g  ->
        match g with
        | Liter n      -> n * Multipliers.one
        | DeciLiter n  -> n * Multipliers.deci
        | MilliLiter n -> n * Multipliers.milli
        | MicroLiter n -> n * Multipliers.micro
    | Time g  ->
        match g with
        | Year n   -> n * Multipliers.year
        | Month n  -> n * Multipliers.month
        | Week n   -> n * Multipliers.week
        | Day n    -> n * Multipliers.day
        | Hour n   -> n * Multipliers.hour
        | Minute n -> n * Multipliers.minute
        | Second n -> n * Multipliers.second
    | Molar g ->
        match g with
        | Mol n      -> n * Multipliers.one
        | MilliMol n -> n * Multipliers.milli
    | InterNatUnit g ->
        match g with
        | MIU n -> n * Multipliers.kilo * Multipliers.kilo
        | IU n  -> n * Multipliers.one
    | Weight g -> 
        match g with
        | WeightKiloGram n -> n * Multipliers.kilo
        | WeightGram n     -> n * Multipliers.one
    | Height g -> 
        match g with
        | HeightMeter n      -> n * Multipliers.one
        | HeightCentiMeter n -> n * Multipliers.centi
    | BSA g -> 
        match g with
        | M2 n -> n * Multipliers.one




let eqsGroup (v, u) =
    

let getMultiplier u =
    let rec get u m = 
        match u with
        | NoUnit -> m 
        | Unit u -> u |> getUnitGroupMultiplier
        | CombiUnit (u1, op, u2) ->
            let m1 = get u1 m
            let m2 = get u2 m

            match op with
            | OpTimes -> m1 * m2 |> get u
            | OpPer  -> m1 / m2 |> get u
            | OpMinus | OpPlus -> m

    get u 1N

let massKg = KiloGram >> Mass >> Unit
let wghtKg = WeightKiloGram >> Weight >> Unit

2N |> massKg |> getMultiplier


let kg10 = 10N |> KiloGram



type Group =
    | MassGroup   of (MassUnit -> Unit)
    | VolumeGroup of (VolumeUnit -> Unit)
    | WeightGroup of (WeightUnit -> Unit)
