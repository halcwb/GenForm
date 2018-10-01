#I __SOURCE_DIRECTORY__

#load "./../../../.paket/load/netstandard2.0/main.group.fsx"

#r "./../../Informedica.GenUtils.Lib/bin/Debug/netstandard2.0/Informedica.GenUtils.Lib.dll"

#time


open Informedica.GenUtils.Lib.BCL
open Informedica.GenUtils.Lib

open Aether
open Aether.Operators



module List =

    let tryFindRest pred xs =
        let rec find x xs notx =
            match xs with
            | [] -> x, notx
            | h::tail ->
                if h |> pred then find (Some h) tail notx
                else find x tail ([h] |> List.append notx)

        find None xs []



module ValueUnit =

    type Unit = string


    /// Value and unit
    type ValueUnit =
        {
            Value : float
            Unit : Unit
        }


    let empty = { Value = 0.; Unit = "" }


    let create v u = { Value = v; Unit = u }


    type ValueUnit with

        static member Value_ :
            (ValueUnit -> float) * (float -> ValueUnit -> ValueUnit) =
            (fun vu -> vu.Value), (fun v vu -> { vu with Value = v })

        static member Unit_ :
            (ValueUnit -> string) * (string -> ValueUnit -> ValueUnit) =
            (fun vu -> vu.Unit), (fun u vu -> { vu with Unit = u })


    let getValue = Optic.get ValueUnit.Value_


    let setValue = Optic.set ValueUnit.Value_ 


    let getUnit = Optic.get ValueUnit.Unit_


    let setUnit = Optic.set ValueUnit.Unit_ 


    let toString vu = 
        sprintf "%A %s" (vu |> getValue) (vu |> getUnit)



module MinMax =


    type ValueUnit = ValueUnit.ValueUnit


    /// Range with min and/or max
    type MinMax =
        {
            Min : Value option
            Max : Value option
        }
    and Value = Inclusive of ValueUnit | Exclusive of ValueUnit


    let create min max = { Min = min; Max = max }
    

    let empty = create None None


    let inclusive v = v |> Inclusive


    let exclusive v = v |> Exclusive


    let min v = { empty with Min = v |> Some }


    let max v = { empty with Max = v |> Some }
    

    let minIncl v = min (v |> inclusive) 


    let minExcl v = min (v |> exclusive) 


    let maxIncl v = max (v |> inclusive) 


    let maxExcl v = max (v |> exclusive) 


    type Value with

        static member Inclusive_ =
            (fun v ->
                match v with
                | Inclusive v_ -> v_ |> Some 
                | Exclusive _  -> None
            ), 
            (fun x v ->
                match v with 
                | Inclusive _ -> x |> Inclusive
                | Exclusive _ -> v
            )


        static member Exclusive_ =
            (fun v ->
                match v with
                | Inclusive _  -> None
                | Exclusive v_ -> v_ |> Some
            ), 
            (fun x v ->
                match v with 
                | Inclusive _ -> v
                | Exclusive _ -> x |> Exclusive
            )


    let getInclusive = Optic.get Value.Inclusive_


    let setInclusive = Optic.set Value.Inclusive_


    let getExclusive = Optic.get Value.Exclusive_


    let setExclusive = Optic.set Value.Exclusive_


    let inclusiveValueLens = Value.Inclusive_ >?> ValueUnit.Value_


    let getInclusiveValue = Optic.get inclusiveValueLens


    let setInclusiveValue = Optic.set inclusiveValueLens 
    

    let inclusiveUnitLens = Value.Inclusive_ >?> ValueUnit.Unit_


    let getInclusiveUnit = Optic.get inclusiveUnitLens


    let setInclusiveUnit = Optic.set inclusiveUnitLens 
    

    let exclusiveValueLens = Value.Exclusive_ >?> ValueUnit.Value_


    let getExclusiveValue = Optic.get exclusiveValueLens


    let setExclusiveValue = Optic.set exclusiveValueLens 
    

    let exclusiveUnitLens = Value.Exclusive_ >?> ValueUnit.Unit_


    let getExclusiveUnit = Optic.get exclusiveUnitLens


    let setExclusiveUnit = Optic.set exclusiveUnitLens 
    

    type MinMax with

        static member Min_ :
            (MinMax -> Value Option) * (Value -> MinMax -> MinMax) =
            (fun mm -> mm.Min), 
            (fun v mm -> { mm with Min = Some v })

        static member Max_ :
            (MinMax -> Value Option) * (Value -> MinMax -> MinMax) =
            (fun mm -> mm.Max), 
            (fun v mm -> { mm with Max = Some v })


    let getMin = Optic.get MinMax.Min_


    let setMin = Optic.set MinMax.Min_


    let inclMinLens = 
        (fun mm -> 
            match mm |> getMin with 
            | Some min -> 
                match min with 
                | Inclusive v -> Some v 
                | _ -> None 
            | None -> None),
        (fun vu mm -> mm |> setMin (vu |> inclusive))


    let getInclMin = Optic.get inclMinLens


    let setInclMin = Optic.set inclMinLens


    let valueInclMinLens = inclMinLens >?> ValueUnit.Value_


    let getValueInclMin = Optic.get valueInclMinLens


    let setValueInclMin = Optic.set valueInclMinLens


    let unitInclMinLens = inclMinLens >?> ValueUnit.Unit_


    let getUnitInclMin = Optic.get unitInclMinLens


    let setUnitInclMin = Optic.set unitInclMinLens


    let exclMinLens = 
        (fun mm -> 
            match mm |> getMin with 
            | Some min -> 
                match min with 
                | Exclusive v -> Some v 
                | _ -> None 
            | None -> None),
        (fun vu mm -> mm |> setMin (vu |> exclusive))


    let getExclMin = Optic.get exclMinLens


    let setExclMin = Optic.set exclMinLens
    

    let valueExclMinLens = exclMinLens >?> ValueUnit.Value_


    let getValueExclMin = Optic.get valueExclMinLens


    let setValueExclMin = Optic.set valueExclMinLens


    let unitExclMinLens = exclMinLens >?> ValueUnit.Unit_


    let getUnitExclMin = Optic.get unitExclMinLens


    let setUnitExclMin = Optic.set unitExclMinLens


    let getMax = Optic.get MinMax.Max_


    let setMax = Optic.set MinMax.Max_


    let inclMaxLens = 
        (fun mm -> 
            match mm |> getMax with 
            | Some max -> 
                match max with 
                | Inclusive v -> Some v 
                | _ -> None 
            | None -> None),
        (fun vu mm -> mm |> setMax (vu |> inclusive))


    let getInclMax = Optic.get inclMaxLens


    let setInclMax = Optic.set inclMaxLens


    let valueInclMaxLens = inclMaxLens >?> ValueUnit.Value_


    let getValueInclMax = Optic.get valueInclMaxLens


    let setValueInclMax = Optic.set valueInclMaxLens


    let unitInclMaxLens = inclMaxLens >?> ValueUnit.Unit_


    let getUnitInclMax = Optic.get unitInclMaxLens


    let setUnitInclMax = Optic.set unitInclMaxLens


    let exclMaxLens = 
        (fun mm -> 
            match mm |> getMax with 
            | Some max -> 
                match max with 
                | Exclusive v -> Some v 
                | _ -> None 
            | None -> None),
        (fun vu mm -> mm |> setMax (vu |> exclusive))


    let getExclMax = Optic.get exclMaxLens


    let setExclMax = Optic.set exclMaxLens
    

    let valueExclMaxLens = exclMaxLens >?> ValueUnit.Value_


    let getValueExclMax = Optic.get valueExclMaxLens


    let setValueExclMax = Optic.set valueExclMaxLens


    let unitExclMaxLens = exclMaxLens >?> ValueUnit.Unit_


    let getUnitExclMax = Optic.get unitExclMaxLens


    let setUnitExclMax = Optic.set unitExclMaxLens
    

    let toString { Min = min; Max = max } =
        let minToString min =
            match min with 
            | Inclusive vu ->
                vu |> ValueUnit.toString |> sprintf "meer dan en gelijk aan %s"
            | Exclusive vu ->
                vu |> ValueUnit.toString |> sprintf "meer dan %s"

        let maxToString min =
            match min with 
            | Inclusive vu ->
                vu |> ValueUnit.toString |> sprintf "tot en met %s"
            | Exclusive vu ->
                vu |> ValueUnit.toString |> sprintf "tot %s"

        match min, max with
        | None, None -> ""
        | Some min_, Some max_ -> 
            sprintf "%s - %s "(min_ |> minToString) (max_ |> maxToString)
        | Some min_, None -> 
            (min_ |> minToString) 
        | None, Some max_ -> 
            (max_ |> maxToString)



module DoseRange =


    type MinMax = MinMax.MinMax


    /// Dose limits
    type DoseRange =
        {
            // Normal limits
            Norm : MinMax
            // Normal limits adjusted by weight
            NormWeight : MinMax
            // Normal limits adjusted by BSA
            NormBSA : MinMax
            // Absolute limits
            Abs : MinMax
            // Absolute limits adjusted by weight
            AbsWeight : MinMax
            // Absolute limits adjusted by BSA
            AbsBSA : MinMax
        }


    let create norm normWght normBSA abs absWght absBSA =
        {
            Norm = norm
            NormWeight = normWght
            NormBSA = normBSA
            Abs = abs
            AbsWeight = absWght
            AbsBSA = absBSA
        }


    let empty = create MinMax.empty MinMax.empty MinMax.empty MinMax.empty MinMax.empty MinMax.empty


    type DoseRange with

        static member Norm_ :
            (DoseRange -> MinMax) * (MinMax -> DoseRange -> DoseRange) =
            (fun dr -> dr.Norm),
            (fun mm dr -> { dr with Norm = mm })

        static member NormWeight_ :
            (DoseRange -> MinMax) * (MinMax -> DoseRange -> DoseRange) =
            (fun dr -> dr.NormWeight),
            (fun mm dr -> { dr with NormWeight = mm })

        static member NormBSA_ :
            (DoseRange -> MinMax) * (MinMax -> DoseRange -> DoseRange) =
            (fun dr -> dr.NormBSA),
            (fun mm dr -> { dr with NormBSA = mm })

        static member Abs_ :
            (DoseRange -> MinMax) * (MinMax -> DoseRange -> DoseRange) =
            (fun dr -> dr.Abs),
            (fun mm dr -> { dr with Abs = mm })

        static member AbsWeight_ :
            (DoseRange -> MinMax) * (MinMax -> DoseRange -> DoseRange) =
            (fun dr -> dr.AbsWeight),
            (fun mm dr -> { dr with AbsWeight = mm })

        static member AbsBSA_ :
            (DoseRange -> MinMax) * (MinMax -> DoseRange -> DoseRange) =
            (fun dr -> dr.AbsBSA),
            (fun mm dr -> { dr with AbsBSA = mm })


    let getNorm = Optic.get DoseRange.Norm_


    let setNorm = Optic.set DoseRange.Norm_


    let minNormLens = DoseRange.Norm_ >-> MinMax.Min_


    let getMinNorm = Optic.get minNormLens


    let setMinNorm = Optic.set minNormLens


    let inclMinNormLens =
        DoseRange.Norm_ >-> (MinMax.inclMinLens) 


    let getInclMinNorm = Optic.get inclMinNormLens 


    let setInclMinNorm = Optic.set inclMinNormLens


    let exclMinNormLens =
        DoseRange.Norm_ >-> (MinMax.exclMinLens) 


    let getExclMinNorm = Optic.get exclMinNormLens 


    let setExclMinNorm = Optic.set exclMinNormLens


    let maxNormLens = DoseRange.Norm_ >-> MinMax.Max_


    let getMaxNorm = Optic.get maxNormLens


    let setMaxNorm = Optic.set maxNormLens


    let inclMaxNormLens =
        DoseRange.Norm_ >-> (MinMax.inclMaxLens) 


    let getInclMaxNorm = Optic.get inclMaxNormLens 


    let setInclMaxNorm = Optic.set inclMaxNormLens


    let exclMaxNormLens =
        DoseRange.Norm_ >-> (MinMax.exclMaxLens) 


    let getExclMaxNorm = Optic.get exclMaxNormLens 


    let setExclMaxNorm = Optic.set exclMaxNormLens


    let getNormWeight = Optic.get DoseRange.NormWeight_


    let setNormWeight = Optic.set DoseRange.NormWeight_


    let minNormWeightLens = DoseRange.NormWeight_ >-> MinMax.Min_


    let getMinNormWeight = Optic.get minNormWeightLens


    let setMinNormWeight = Optic.set minNormWeightLens


    let inclMinNormWeightLens =
        DoseRange.NormWeight_ >-> (MinMax.inclMinLens) 


    let getInclMinNormWeight = Optic.get inclMinNormWeightLens 


    let setInclMinNormWeight = Optic.set inclMinNormWeightLens


    let exclMinNormWeightLens =
        DoseRange.NormWeight_ >-> (MinMax.exclMinLens) 


    let getExclMinNormWeight = Optic.get exclMinNormWeightLens 


    let setExclMinNormWeight = Optic.set exclMinNormWeightLens


    let maxNormWeightLens = DoseRange.NormWeight_ >-> MinMax.Max_


    let getMaxNormWeight = Optic.get maxNormWeightLens


    let setMaxNormWeight = Optic.set maxNormWeightLens


    let inclMaxNormWeightLens =
        DoseRange.NormWeight_ >-> (MinMax.inclMaxLens) 


    let getInclMaxNormWeight = Optic.get inclMaxNormWeightLens 


    let setInclMaxNormWeight = Optic.set inclMaxNormWeightLens


    let exclMaxNormWeightLens =
        DoseRange.NormWeight_ >-> (MinMax.exclMaxLens) 


    let getExclMaxNormWeight = Optic.get exclMaxNormWeightLens 


    let setExclMaxNormWeight = Optic.set exclMaxNormWeightLens


    let getNormBSA = Optic.get DoseRange.NormBSA_


    let setNormBSA = Optic.set DoseRange.NormBSA_


    let minNormBSALens = DoseRange.NormBSA_ >-> MinMax.Min_


    let getMinNormBSA = Optic.get minNormBSALens


    let setMinNormBSA = Optic.set minNormBSALens


    let inclMinNormBSALens =
        DoseRange.NormBSA_ >-> (MinMax.inclMinLens) 


    let getInclMinNormBSA = Optic.get inclMinNormBSALens 


    let setInclMinNormBSA = Optic.set inclMinNormBSALens


    let exclMinNormBSALens =
        DoseRange.NormBSA_ >-> (MinMax.exclMinLens) 


    let getExclMinNormBSA = Optic.get exclMinNormBSALens 


    let setExclMinNormBSA = Optic.set exclMinNormBSALens


    let maxNormBSALens = DoseRange.NormBSA_ >-> MinMax.Max_


    let getMaxNormBSA = Optic.get maxNormBSALens


    let setMaxNormBSA = Optic.set maxNormBSALens


    let inclMaxNormBSALens =
        DoseRange.NormBSA_ >-> (MinMax.inclMaxLens) 


    let getInclMaxNormBSA = Optic.get inclMaxNormBSALens 


    let setInclMaxNormBSA = Optic.set inclMaxNormBSALens


    let exclMaxNormBSALens =
        DoseRange.NormBSA_ >-> (MinMax.exclMaxLens) 


    let getExclMaxNormBSA = Optic.get exclMaxNormBSALens 


    let setExclMaxNormBSA = Optic.set exclMaxNormBSALens


    let getAbs = Optic.get DoseRange.Abs_


    let setAbs = Optic.set DoseRange.Abs_


    let minAbsLens = DoseRange.Abs_ >-> MinMax.Min_


    let getMinAbs = Optic.get minAbsLens


    let setMinAbs = Optic.set minAbsLens


    let inclMinAbsLens =
        DoseRange.Abs_ >-> (MinMax.inclMinLens) 


    let getInclMinAbs = Optic.get inclMinAbsLens 


    let setInclMinAbs = Optic.set inclMinAbsLens


    let exclMinAbsLens =
        DoseRange.Abs_ >-> (MinMax.exclMinLens) 


    let getExclMinAbs = Optic.get exclMinAbsLens 


    let setExclMinAbs = Optic.set exclMinAbsLens


    let maxAbsLens = DoseRange.Abs_ >-> MinMax.Max_


    let getMaxAbs = Optic.get maxAbsLens


    let setMaxAbs = Optic.set maxAbsLens


    let inclMaxAbsLens =
        DoseRange.Abs_ >-> (MinMax.inclMaxLens) 


    let getInclMaxAbs = Optic.get inclMaxAbsLens 


    let setInclMaxAbs = Optic.set inclMaxAbsLens


    let exclMaxAbsLens =
        DoseRange.Abs_ >-> (MinMax.exclMaxLens) 


    let getExclMaxAbs = Optic.get exclMaxAbsLens 


    let setExclMaxAbs = Optic.set exclMaxAbsLens


    let getAbsWeight = Optic.get DoseRange.AbsWeight_


    let setAbsWeight = Optic.set DoseRange.AbsWeight_


    let minAbsWeightLens = DoseRange.AbsWeight_ >-> MinMax.Min_


    let getMinAbsWeight = Optic.get minAbsWeightLens


    let setMinAbsWeight = Optic.set minAbsWeightLens


    let inclMinAbsWeightLens =
        DoseRange.AbsWeight_ >-> (MinMax.inclMinLens) 


    let getInclMinAbsWeight = Optic.get inclMinAbsWeightLens 


    let setInclMinAbsWeight = Optic.set inclMinAbsWeightLens


    let exclMinAbsWeightLens =
        DoseRange.AbsWeight_ >-> (MinMax.exclMinLens) 


    let getExclMinAbsWeight = Optic.get exclMinAbsWeightLens 


    let setExclMinAbsWeight = Optic.set exclMinAbsWeightLens


    let maxAbsWeightLens = DoseRange.AbsWeight_ >-> MinMax.Max_


    let getMaxAbsWeight = Optic.get maxAbsWeightLens


    let setMaxAbsWeight = Optic.set maxAbsWeightLens


    let inclMaxAbsWeightLens =
        DoseRange.AbsWeight_ >-> (MinMax.inclMaxLens) 


    let getInclMaxAbsWeight = Optic.get inclMaxAbsWeightLens 


    let setInclMaxAbsWeight = Optic.set inclMaxAbsWeightLens


    let exclMaxAbsWeightLens =
        DoseRange.AbsWeight_ >-> (MinMax.exclMaxLens) 


    let getExclMaxAbsWeight = Optic.get exclMaxAbsWeightLens 


    let setExclMaxAbsWeight = Optic.set exclMaxAbsWeightLens


    let getAbsBSA = Optic.get DoseRange.AbsBSA_


    let setAbsBSA = Optic.set DoseRange.AbsBSA_


    let minAbsBSALens = DoseRange.AbsBSA_ >-> MinMax.Min_


    let getMinAbsBSA = Optic.get minAbsBSALens


    let setMinAbsBSA = Optic.set minAbsBSALens


    let inclMinAbsBSALens =
        DoseRange.AbsBSA_ >-> (MinMax.inclMinLens) 


    let getInclMinAbsBSA = Optic.get inclMinAbsBSALens 


    let setInclMinAbsBSA = Optic.set inclMinAbsBSALens


    let exclMinAbsBSALens =
        DoseRange.AbsBSA_ >-> (MinMax.exclMinLens) 


    let getExclMinAbsBSA = Optic.get exclMinAbsBSALens 


    let setExclMinAbsBSA = Optic.set exclMinAbsBSALens


    let maxAbsBSALens = DoseRange.AbsBSA_ >-> MinMax.Max_


    let getMaxAbsBSA = Optic.get maxAbsBSALens


    let setMaxAbsBSA = Optic.set maxAbsBSALens


    let inclMaxAbsBSALens =
        DoseRange.AbsBSA_ >-> (MinMax.inclMaxLens) 


    let getInclMaxAbsBSA = Optic.get inclMaxAbsBSALens 


    let setInclMaxAbsBSA = Optic.set inclMaxAbsBSALens


    let exclMaxAbsBSALens =
        DoseRange.AbsBSA_ >-> (MinMax.exclMaxLens) 


    let getExclMaxAbsBSA = Optic.get exclMaxAbsBSALens 


    let setExclMaxAbsBSA = Optic.set exclMaxAbsBSALens



module Dosage =


    type DoseRange = DoseRange.DoseRange


    /// Dosage
    type Dosage =
        {
            Name : string
            /// Dosage at the start
            StartDosage : DoseRange
            /// Dosage per administration
            SingleDosage : DoseRange
            /// Dosage rate
            RateDosage : DoseRange * RateUnit
            /// Total dosage per time period
            TotalDosage : Frequencies * DoseRange * TimeUnit
        }
    and Frequencies = int list
    and TimeUnit = string
    and RateUnit = string


    let create nm start single rate total =
        {
            Name = nm
            StartDosage = start
            SingleDosage = single
            RateDosage = rate
            TotalDosage = total
        }


    let empty = create "" DoseRange.empty DoseRange.empty (DoseRange.empty, "") ([], DoseRange.empty, "" )


    type Dosage with

        static member StartDosage_ :
            (Dosage -> DoseRange) * (DoseRange -> Dosage -> Dosage) =
            (fun d -> d.StartDosage),
            (fun dr d -> { d with StartDosage = dr })

        static member SingleDosage_ :
            (Dosage -> DoseRange) * (DoseRange -> Dosage -> Dosage) =
            (fun d -> d.SingleDosage),
            (fun dr d -> { d with SingleDosage = dr })

        static member RateDosage_ :
            (Dosage -> (DoseRange * RateUnit)) * ((DoseRange * RateUnit) -> Dosage -> Dosage) =
            (fun d -> d.RateDosage),
            (fun dr d -> { d with RateDosage = dr })

        static member TotalDosage_ :
            (Dosage -> (Frequencies * DoseRange * TimeUnit)) * ((Frequencies * DoseRange * TimeUnit) -> Dosage -> Dosage) =
            (fun d -> d.TotalDosage),
            (fun dt d -> { d with TotalDosage = dt })


    let getStartDosage = Optic.get Dosage.StartDosage_


    let setStartDosage = Optic.set Dosage.StartDosage_


    let inclMinNormStartDosagePrism =
        Dosage.StartDosage_ >-> DoseRange.inclMinNormLens


    let getInclMinNormStartDosage = Optic.get inclMinNormStartDosagePrism


    let setInclMinNormStartDosage = Optic.set inclMinNormStartDosagePrism


    let exclMinNormStartDosagePrism =
        Dosage.StartDosage_ >-> DoseRange.exclMinNormLens


    let getExclMinNormStartDosage = Optic.get exclMinNormStartDosagePrism


    let setExclMinNormStartDosage = Optic.set exclMinNormStartDosagePrism


    let inclMaxNormStartDosagePrism =
        Dosage.StartDosage_ >-> DoseRange.inclMaxNormLens


    let getInclMaxNormStartDosage = Optic.get inclMaxNormStartDosagePrism


    let setInclMaxNormStartDosage = Optic.set inclMaxNormStartDosagePrism


    let exclMaxNormStartDosagePrism =
        Dosage.StartDosage_ >-> DoseRange.exclMaxNormLens


    let getExclMaxNormStartDosage = Optic.get exclMaxNormStartDosagePrism


    let setExclMaxNormStartDosage = Optic.set exclMaxNormStartDosagePrism


    let inclMinNormWeightStartDosagePrism =
        Dosage.StartDosage_ >-> DoseRange.inclMinNormWeightLens


    let getInclMinNormWeightStartDosage = Optic.get inclMinNormWeightStartDosagePrism


    let setInclMinNormWeightStartDosage = Optic.set inclMinNormWeightStartDosagePrism


    let exclMinNormWeightStartDosagePrism =
        Dosage.StartDosage_ >-> DoseRange.exclMinNormWeightLens


    let getExclMinNormWeightStartDosage = Optic.get exclMinNormWeightStartDosagePrism


    let setExclMinNormWeightStartDosage = Optic.set exclMinNormWeightStartDosagePrism


    let inclMaxNormWeightStartDosagePrism =
        Dosage.StartDosage_ >-> DoseRange.inclMaxNormWeightLens


    let getInclMaxNormWeightStartDosage = Optic.get inclMaxNormWeightStartDosagePrism


    let setInclMaxNormWeightStartDosage = Optic.set inclMaxNormWeightStartDosagePrism


    let exclMaxNormWeightStartDosagePrism =
        Dosage.StartDosage_ >-> DoseRange.exclMaxNormWeightLens


    let getExclMaxNormWeightStartDosage = Optic.get exclMaxNormWeightStartDosagePrism


    let setExclMaxNormWeightStartDosage = Optic.set exclMaxNormWeightStartDosagePrism


    let inclMinNormBSAStartDosagePrism =
        Dosage.StartDosage_ >-> DoseRange.inclMinNormBSALens


    let getInclMinNormBSAStartDosage = Optic.get inclMinNormBSAStartDosagePrism


    let setInclMinNormBSAStartDosage = Optic.set inclMinNormBSAStartDosagePrism


    let exclMinNormBSAStartDosagePrism =
        Dosage.StartDosage_ >-> DoseRange.exclMinNormBSALens


    let getExclMinNormBSAStartDosage = Optic.get exclMinNormBSAStartDosagePrism


    let setExclMinNormBSAStartDosage = Optic.set exclMinNormBSAStartDosagePrism


    let inclMaxNormBSAStartDosagePrism =
        Dosage.StartDosage_ >-> DoseRange.inclMaxNormBSALens


    let getInclMaxNormBSAStartDosage = Optic.get inclMaxNormBSAStartDosagePrism


    let setInclMaxNormBSAStartDosage = Optic.set inclMaxNormBSAStartDosagePrism


    let exclMaxNormBSAStartDosagePrism =
        Dosage.StartDosage_ >-> DoseRange.exclMaxNormBSALens


    let getExclMaxNormBSAStartDosage = Optic.get exclMaxNormBSAStartDosagePrism


    let setExclMaxNormBSAStartDosage = Optic.set exclMaxNormBSAStartDosagePrism
    

    let getSingleDosage = Optic.get Dosage.SingleDosage_


    let setSingleDosage = Optic.set Dosage.SingleDosage_


    let inclMinNormSingleDosagePrism =
        Dosage.SingleDosage_ >-> DoseRange.inclMinNormLens


    let getInclMinNormSingleDosage = Optic.get inclMinNormSingleDosagePrism


    let setInclMinNormSingleDosage = Optic.set inclMinNormSingleDosagePrism


    let exclMinNormSingleDosagePrism =
        Dosage.SingleDosage_ >-> DoseRange.exclMinNormLens


    let getExclMinNormSingleDosage = Optic.get exclMinNormSingleDosagePrism


    let setExclMinNormSingleDosage = Optic.set exclMinNormSingleDosagePrism


    let inclMaxNormSingleDosagePrism =
        Dosage.SingleDosage_ >-> DoseRange.inclMaxNormLens


    let getInclMaxNormSingleDosage = Optic.get inclMaxNormSingleDosagePrism


    let setInclMaxNormSingleDosage = Optic.set inclMaxNormSingleDosagePrism


    let exclMaxNormSingleDosagePrism =
        Dosage.SingleDosage_ >-> DoseRange.exclMaxNormLens


    let getExclMaxNormSingleDosage = Optic.get exclMaxNormSingleDosagePrism


    let setExclMaxNormSingleDosage = Optic.set exclMaxNormSingleDosagePrism


    let inclMinNormWeightSingleDosagePrism =
        Dosage.SingleDosage_ >-> DoseRange.inclMinNormWeightLens


    let getInclMinNormWeightSingleDosage = Optic.get inclMinNormWeightSingleDosagePrism


    let setInclMinNormWeightSingleDosage = Optic.set inclMinNormWeightSingleDosagePrism


    let exclMinNormWeightSingleDosagePrism =
        Dosage.SingleDosage_ >-> DoseRange.exclMinNormWeightLens


    let getExclMinNormWeightSingleDosage = Optic.get exclMinNormWeightSingleDosagePrism


    let setExclMinNormWeightSingleDosage = Optic.set exclMinNormWeightSingleDosagePrism


    let inclMaxNormWeightSingleDosagePrism =
        Dosage.SingleDosage_ >-> DoseRange.inclMaxNormWeightLens


    let getInclMaxNormWeightSingleDosage = Optic.get inclMaxNormWeightSingleDosagePrism


    let setInclMaxNormWeightSingleDosage = Optic.set inclMaxNormWeightSingleDosagePrism


    let exclMaxNormWeightSingleDosagePrism =
        Dosage.SingleDosage_ >-> DoseRange.exclMaxNormWeightLens


    let getExclMaxNormWeightSingleDosage = Optic.get exclMaxNormWeightSingleDosagePrism


    let setExclMaxNormWeightSingleDosage = Optic.set exclMaxNormWeightSingleDosagePrism


    let inclMinNormBSASingleDosagePrism =
        Dosage.SingleDosage_ >-> DoseRange.inclMinNormBSALens


    let getInclMinNormBSASingleDosage = Optic.get inclMinNormBSASingleDosagePrism


    let setInclMinNormBSASingleDosage = Optic.set inclMinNormBSASingleDosagePrism


    let exclMinNormBSASingleDosagePrism =
        Dosage.SingleDosage_ >-> DoseRange.exclMinNormBSALens


    let getExclMinNormBSASingleDosage = Optic.get exclMinNormBSASingleDosagePrism


    let setExclMinNormBSASingleDosage = Optic.set exclMinNormBSASingleDosagePrism


    let inclMaxNormBSASingleDosagePrism =
        Dosage.SingleDosage_ >-> DoseRange.inclMaxNormBSALens


    let getInclMaxNormBSASingleDosage = Optic.get inclMaxNormBSASingleDosagePrism


    let setInclMaxNormBSASingleDosage = Optic.set inclMaxNormBSASingleDosagePrism


    let exclMaxNormBSASingleDosagePrism =
        Dosage.SingleDosage_ >-> DoseRange.exclMaxNormBSALens


    let getExclMaxNormBSASingleDosage = Optic.get exclMaxNormBSASingleDosagePrism


    let setExclMaxNormBSASingleDosage = Optic.set exclMaxNormBSASingleDosagePrism


    let getRateDosage = Optic.get Dosage.RateDosage_


    let setRateDosage = Optic.set Dosage.RateDosage_


    let doseRangeRateDosagePrism :
        (Dosage -> DoseRange) * (DoseRange -> Dosage -> Dosage) = 
        (fun ds -> ds.RateDosage |> fst) ,
        (fun dr ds -> { ds with RateDosage = (dr, ds.RateDosage |> snd) } )


    let inclMinNormRateDosagePrism =
        doseRangeRateDosagePrism >-> DoseRange.inclMinNormLens


    let getInclMinNormRateDosage = Optic.get inclMinNormRateDosagePrism


    let setInclMinNormRateDosage = Optic.set inclMinNormRateDosagePrism


    let exclMinNormRateDosagePrism =
        doseRangeRateDosagePrism >-> DoseRange.exclMinNormLens


    let getExclMinNormRateDosage = Optic.get exclMinNormRateDosagePrism


    let setExclMinNormRateDosage = Optic.set exclMinNormRateDosagePrism


    let inclMaxNormRateDosagePrism =
        doseRangeRateDosagePrism >-> DoseRange.inclMaxNormLens


    let getInclMaxNormRateDosage = Optic.get inclMaxNormRateDosagePrism


    let setInclMaxNormRateDosage = Optic.set inclMaxNormRateDosagePrism


    let exclMaxNormRateDosagePrism =
        doseRangeRateDosagePrism >-> DoseRange.exclMaxNormLens


    let getExclMaxNormRateDosage = Optic.get exclMaxNormRateDosagePrism


    let setExclMaxNormRateDosage = Optic.set exclMaxNormRateDosagePrism


    let inclMinNormWeightRateDosagePrism =
        doseRangeRateDosagePrism >-> DoseRange.inclMinNormWeightLens


    let getInclMinNormWeightRateDosage = Optic.get inclMinNormWeightRateDosagePrism


    let setInclMinNormWeightRateDosage = Optic.set inclMinNormWeightRateDosagePrism


    let exclMinNormWeightRateDosagePrism =
        doseRangeRateDosagePrism >-> DoseRange.exclMinNormWeightLens


    let getExclMinNormWeightRateDosage = Optic.get exclMinNormWeightRateDosagePrism


    let setExclMinNormWeightRateDosage = Optic.set exclMinNormWeightRateDosagePrism


    let inclMaxNormWeightRateDosagePrism =
        doseRangeRateDosagePrism >-> DoseRange.inclMaxNormWeightLens


    let getInclMaxNormWeightRateDosage = Optic.get inclMaxNormWeightRateDosagePrism


    let setInclMaxNormWeightRateDosage = Optic.set inclMaxNormWeightRateDosagePrism


    let exclMaxNormWeightRateDosagePrism =
        doseRangeRateDosagePrism >-> DoseRange.exclMaxNormWeightLens


    let getExclMaxNormWeightRateDosage = Optic.get exclMaxNormWeightRateDosagePrism


    let setExclMaxNormWeightRateDosage = Optic.set exclMaxNormWeightRateDosagePrism


    let inclMinNormBSARateDosagePrism =
        doseRangeRateDosagePrism >-> DoseRange.inclMinNormBSALens


    let getInclMinNormBSARateDosage = Optic.get inclMinNormBSARateDosagePrism


    let setInclMinNormBSARateDosage = Optic.set inclMinNormBSARateDosagePrism


    let exclMinNormBSARateDosagePrism =
        doseRangeRateDosagePrism >-> DoseRange.exclMinNormBSALens


    let getExclMinNormBSARateDosage = Optic.get exclMinNormBSARateDosagePrism


    let setExclMinNormBSARateDosage = Optic.set exclMinNormBSARateDosagePrism


    let inclMaxNormBSARateDosagePrism =
        doseRangeRateDosagePrism >-> DoseRange.inclMaxNormBSALens


    let getInclMaxNormBSARateDosage = Optic.get inclMaxNormBSARateDosagePrism


    let setInclMaxNormBSARateDosage = Optic.set inclMaxNormBSARateDosagePrism


    let exclMaxNormBSARateDosagePrism =
        doseRangeRateDosagePrism >-> DoseRange.exclMaxNormBSALens


    let getExclMaxNormBSARateDosage = Optic.get exclMaxNormBSARateDosagePrism


    let setExclMaxNormBSARateDosage = Optic.set exclMaxNormBSARateDosagePrism



    let getTotalDosage = Optic.get Dosage.TotalDosage_


    let setTotalDosage = Optic.set Dosage.TotalDosage_


    let doseRangeTotalDosagePrism :
        (Dosage -> DoseRange) * (DoseRange -> Dosage -> Dosage) = 
        (fun ds -> let (_, dr, _) = ds.TotalDosage in dr) ,
        (fun dr ds -> 
            let (fr, _, tu) = ds.TotalDosage
            { ds with TotalDosage = (fr, dr, tu) } 
        )


    let inclMinNormTotalDosagePrism =
        doseRangeTotalDosagePrism >-> DoseRange.inclMinNormLens


    let getInclMinNormTotalDosage = Optic.get inclMinNormTotalDosagePrism


    let setInclMinNormTotalDosage = Optic.set inclMinNormTotalDosagePrism


    let exclMinNormTotalDosagePrism =
        doseRangeTotalDosagePrism >-> DoseRange.exclMinNormLens


    let getExclMinNormTotalDosage = Optic.get exclMinNormTotalDosagePrism


    let setExclMinNormTotalDosage = Optic.set exclMinNormTotalDosagePrism


    let inclMaxNormTotalDosagePrism =
        doseRangeTotalDosagePrism >-> DoseRange.inclMaxNormLens


    let getInclMaxNormTotalDosage = Optic.get inclMaxNormTotalDosagePrism


    let setInclMaxNormTotalDosage = Optic.set inclMaxNormTotalDosagePrism


    let exclMaxNormTotalDosagePrism =
        doseRangeTotalDosagePrism >-> DoseRange.exclMaxNormLens


    let getExclMaxNormTotalDosage = Optic.get exclMaxNormTotalDosagePrism


    let setExclMaxNormTotalDosage = Optic.set exclMaxNormTotalDosagePrism


    let inclMinNormWeightTotalDosagePrism =
        doseRangeTotalDosagePrism >-> DoseRange.inclMinNormWeightLens


    let getInclMinNormWeightTotalDosage = Optic.get inclMinNormWeightTotalDosagePrism


    let setInclMinNormWeightTotalDosage = Optic.set inclMinNormWeightTotalDosagePrism


    let exclMinNormWeightTotalDosagePrism =
        doseRangeTotalDosagePrism >-> DoseRange.exclMinNormWeightLens


    let getExclMinNormWeightTotalDosage = Optic.get exclMinNormWeightTotalDosagePrism


    let setExclMinNormWeightTotalDosage = Optic.set exclMinNormWeightTotalDosagePrism


    let inclMaxNormWeightTotalDosagePrism =
        doseRangeTotalDosagePrism >-> DoseRange.inclMaxNormWeightLens


    let getInclMaxNormWeightTotalDosage = Optic.get inclMaxNormWeightTotalDosagePrism


    let setInclMaxNormWeightTotalDosage = Optic.set inclMaxNormWeightTotalDosagePrism


    let exclMaxNormWeightTotalDosagePrism =
        doseRangeTotalDosagePrism >-> DoseRange.exclMaxNormWeightLens


    let getExclMaxNormWeightTotalDosage = Optic.get exclMaxNormWeightTotalDosagePrism


    let setExclMaxNormWeightTotalDosage = Optic.set exclMaxNormWeightTotalDosagePrism


    let inclMinNormBSATotalDosagePrism =
        doseRangeTotalDosagePrism >-> DoseRange.inclMinNormBSALens


    let getInclMinNormBSATotalDosage = Optic.get inclMinNormBSATotalDosagePrism


    let setInclMinNormBSATotalDosage = Optic.set inclMinNormBSATotalDosagePrism


    let exclMinNormBSATotalDosagePrism =
        doseRangeTotalDosagePrism >-> DoseRange.exclMinNormBSALens


    let getExclMinNormBSATotalDosage = Optic.get exclMinNormBSATotalDosagePrism


    let setExclMinNormBSATotalDosage = Optic.set exclMinNormBSATotalDosagePrism


    let inclMaxNormBSATotalDosagePrism =
        doseRangeTotalDosagePrism >-> DoseRange.inclMaxNormBSALens


    let getInclMaxNormBSATotalDosage = Optic.get inclMaxNormBSATotalDosagePrism


    let setInclMaxNormBSATotalDosage = Optic.set inclMaxNormBSATotalDosagePrism


    let exclMaxNormBSATotalDosagePrism =
        doseRangeTotalDosagePrism >-> DoseRange.exclMaxNormBSALens


    let getExclMaxNormBSATotalDosage = Optic.get exclMaxNormBSATotalDosagePrism


    let setExclMaxNormBSATotalDosage = Optic.set exclMaxNormBSATotalDosagePrism



module Patient = 


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
            

    let getGestAge = Optic.get Patient.GestAge_


    let setGestAge = Optic.set Patient.GestAge_


    let inclMinGestAge =
        Patient.GestAge_ >-> MinMax.inclMinLens


    let getInclMinGestAge = Optic.get inclMinGestAge


    let setInclMinGestAge = Optic.set inclMinGestAge


    let exclMinGestAge =
        Patient.GestAge_ >-> MinMax.exclMinLens


    let getExclMinGestAge = Optic.get exclMinGestAge


    let setExclMinGestAge = Optic.set exclMinGestAge


    let inclMaxGestAge =
        Patient.GestAge_ >-> MinMax.inclMaxLens


    let getInclMaxGestAge = Optic.get inclMaxGestAge


    let setInclMaxGestAge = Optic.set inclMaxGestAge


    let exclMaxGestAge =
        Patient.GestAge_ >-> MinMax.exclMaxLens


    let getExclMaxGestAge = Optic.get exclMaxGestAge


    let setExclMaxGestAge = Optic.set exclMaxGestAge


    let getAge = Optic.get Patient.Age_


    let setAge = Optic.set Patient.Age_


    let inclMinAge =
        Patient.Age_ >-> MinMax.inclMinLens


    let getInclMinAge = Optic.get inclMinAge


    let setInclMinAge = Optic.set inclMinAge


    let exclMinAge =
        Patient.Age_ >-> MinMax.exclMinLens


    let getExclMinAge = Optic.get exclMinAge


    let setExclMinAge = Optic.set exclMinAge


    let inclMaxAge =
        Patient.Age_ >-> MinMax.inclMaxLens


    let getInclMaxAge = Optic.get inclMaxAge


    let setInclMaxAge = Optic.set inclMaxAge


    let exclMaxAge =
        Patient.Age_ >-> MinMax.exclMaxLens


    let getExclMaxAge = Optic.get exclMaxAge


    let setExclMaxAge = Optic.set exclMaxAge


    let getWeight = Optic.get Patient.Weight_


    let setWeight = Optic.set Patient.Weight_


    let inclMinWeight =
        Patient.Weight_ >-> MinMax.inclMinLens


    let getInclMinWeight = Optic.get inclMinWeight


    let setInclMinWeight = Optic.set inclMinWeight


    let exclMinWeight =
        Patient.Weight_ >-> MinMax.exclMinLens


    let getExclMinWeight = Optic.get exclMinWeight


    let setExclMinWeight = Optic.set exclMinWeight


    let inclMaxWeight =
        Patient.Weight_ >-> MinMax.inclMaxLens


    let getInclMaxWeight = Optic.get inclMaxWeight


    let setInclMaxWeight = Optic.set inclMaxWeight


    let exclMaxWeight =
        Patient.Weight_ >-> MinMax.exclMaxLens


    let getExclMaxWeight = Optic.get exclMaxWeight


    let setExclMaxWeight = Optic.set exclMaxWeight


    let getBSA = Optic.get Patient.BSA_


    let setBSA = Optic.set Patient.BSA_


    let inclMinBSA =
        Patient.BSA_ >-> MinMax.inclMinLens


    let getInclMinBSA = Optic.get inclMinBSA


    let setInclMinBSA = Optic.set inclMinBSA


    let exclMinBSA =
        Patient.BSA_ >-> MinMax.exclMinLens


    let getExclMinBSA = Optic.get exclMinBSA


    let setExclMinBSA = Optic.set exclMinBSA


    let inclMaxBSA =
        Patient.BSA_ >-> MinMax.inclMaxLens


    let getInclMaxBSA = Optic.get inclMaxBSA


    let setInclMaxBSA = Optic.set inclMaxBSA


    let exclMaxBSA =
        Patient.BSA_ >-> MinMax.exclMaxLens


    let getExclMaxBSA = Optic.get exclMaxBSA


    let setExclMaxBSA = Optic.set exclMaxBSA


    let getGender = Optic.get Patient.Gender_
    
    
    let setGender = Optic.set Patient.Gender_



module DoseRule =
    open System.Configuration


    type Dosage = Dosage.Dosage
    type SubstanceDosage = Dosage
    type ShapeDosage = Dosage
    type MinMax = MinMax.MinMax
    type Patient = Patient.Patient


    /// Doserule
    type DoseRule =
        {   
            // Generic the doserule applies to
            Generic : string
            // The ATC code
            ATC : string
            // ATCTherapyGroup the doserule applies to
            ATCTherapyGroup : string
            // ATCTherapySubGroup the doserule applies to
            ATCTherapySubGroup : string
            // The generic group the doserule applies to
            GenericGroup : string
            // The generic subgroup the doserule applies to
            GenericSubGroup : string
            // TradeProducts the doserule applies to
            TradeProducts : string list
            // GenericProducts the doserule applies to
            GenericProducts : string list
            // Indication the doserule applies to
            IndicationsDosages : IndicationDosage list
        }
    and IndicationDosage =
        {
            Indications : string list
            RouteDosages : RouteDosage list
        }
    and RouteDosage =
        {
            Route : string
            PatientDosages : PatientDosage list
        } 
    and PatientDosage =
        {
            Patient : Patient
            // Dosage of the shape
            ShapeDosage : ShapeDosage Option
            // List of substances that have a dosage
            SubstanceDosages : SubstanceDosage list
        }


    let doseRule gen atc thg sub ggp gsg tps gps idl =
        {   
            Generic = gen
            ATC = atc
            ATCTherapyGroup = thg
            ATCTherapySubGroup = sub
            GenericGroup = ggp
            GenericSubGroup = gsg
            TradeProducts = tps
            GenericProducts = gps
            IndicationsDosages = idl
        }


    let createIndicationsDosages inds dr =
        { Indications = inds; RouteDosages = [] }::dr.IndicationsDosages


    let addIndications inds (dr : DoseRule) =
        {
            dr with 
                IndicationsDosages =
                    dr |> createIndicationsDosages inds
        }


    let getIndicationsDosage inds (dr : DoseRule) = 
        dr.IndicationsDosages  |> List.tryFindRest (fun x -> x.Indications = inds)


    let createRouteDosages rt (inddos : IndicationDosage) =
        { 
            inddos with 
                RouteDosages = [ { Route = rt; PatientDosages = [] } ] 
        }        

    let getRouteDosage inds rt dr =
        dr
        |> getIndicationsDosage inds
        |> fst
        |> (fun id ->
            match id with 
            | Some id -> 
                id.RouteDosages 
                |> List.tryFindRest (fun x -> x.Route = rt)
            | None -> None, []
        )
 
    type PatientDosage with
    
        static member Patient_ :
            (PatientDosage -> Patient) * (Patient -> PatientDosage -> PatientDosage) =
            (fun pd -> pd.Patient) ,
            (fun pat pd -> { pd with Patient = pat })
 
        
    type RouteDosage with
    
        static member Route_ :
            (RouteDosage -> string) * (string -> RouteDosage -> RouteDosage) =
            (fun rd -> rd.Route) ,
            (fun s rd -> { rd with Route = s })
            
        static member PatientDosages_ :
            (RouteDosage -> PatientDosage list) * (PatientDosage list -> RouteDosage -> RouteDosage) =
            (fun rd -> rd.PatientDosages) ,
            (fun pdl rd -> { rd with PatientDosages = pdl })            
            
        
    type IndicationDosage with
    
        static member Indications_ :
            (IndicationDosage -> string list) * (string list -> IndicationDosage -> IndicationDosage) =
            (fun inds -> inds.Indications) ,
            (fun sl inds -> { inds with Indications = sl })
    
        static member RouteDosages_ :
            (IndicationDosage -> RouteDosage list) * (RouteDosage list -> IndicationDosage -> IndicationDosage) =
            (fun inds -> inds.RouteDosages) ,
            (fun rdl inds -> { inds with RouteDosages = rdl })

    
    type DoseRule with
    
        static member Generic_ :
            (DoseRule -> string) * (string -> DoseRule -> DoseRule) =
            (fun dr -> dr.Generic),
            (fun s dr -> { dr with Generic = s })

        static member IndicationDosages_ :
            (DoseRule -> IndicationDosage list) * (IndicationDosage list -> DoseRule -> DoseRule) =
            (fun dr -> dr.IndicationsDosages) ,
            (fun inds dr -> { dr with IndicationsDosages = inds })


    let getGeneric = Optic.get DoseRule.Generic_
    
    
    let setGeneric = Optic.set DoseRule.Generic_

    
    let getIndicationsDosages = Optic.get DoseRule.IndicationDosages_
    
    
    let setIndicationsDosages = Optic.set DoseRule.IndicationDosages_
    
    
    let indDosIndicationsPrism n =
        DoseRule.IndicationDosages_ >-> List.pos_ n >?> IndicationDosage.Indications_


    let getIndications n dr = 
        match dr |> Optic.get (indDosIndicationsPrism n) with 
        | Some ids -> ids
        | None     -> []
    
    
    let setIndications n = Optic.set (indDosIndicationsPrism n)
    
    
    let indDosDosagesLens n =
        DoseRule.IndicationDosages_ >-> List.pos_ n >?> IndicationDosage.RouteDosages_


    let getRouteDosages inds (dr : DoseRule) =
        match 
            dr.IndicationsDosages
            |> List.tryFindIndex (fun id -> id.Indications = inds) with 
        | Some n -> 
            match dr |> Optic.get (indDosDosagesLens n) with
            | Some drs -> drs
            | None -> []
        | None -> []
        
    
    let setRouteDosages inds rds (dr : DoseRule) =
        match 
            dr.IndicationsDosages
            |> List.tryFindIndex (fun id -> id.Indications = inds) with 
            | Some n -> dr |> Optic.set (indDosDosagesLens n) rds
            | None -> dr
            
        
    let indxIndications inds (dr : DoseRule) =
        dr.IndicationsDosages
        |> List.tryFindIndex (fun id -> id.Indications = inds)       
        
    
    let addRoute inds rt (dr : DoseRule) =
        let rds = [ { Route = rt; PatientDosages = [] } ]
        
        match 
            dr |> indxIndications inds with 
            | Some n -> 
                dr 
                |> Optic.set (indDosDosagesLens n) (dr |> getRouteDosages inds |> List.append rds)
            | None -> dr


    let routeDosPatientDosagesPrism n1 n2 =
        (indDosDosagesLens n1) >?> List.pos_ n2 >?> RouteDosage.PatientDosages_
        

    let indxRoute rt (ind : IndicationDosage) =
        ind.RouteDosages
        |> List.tryFindIndex (fun id -> id.Route = rt)       
        
        
    let getRoutePatientDosages inds rt (dr : DoseRule) =
        let nInd = dr |> indxIndications inds
        
        match nInd with 
        | Some n1 -> 
            match 
                dr.IndicationsDosages.[n1]
                |> indxRoute rt with 
            | Some n2 ->
                match dr 
                      |> Optic.get (routeDosPatientDosagesPrism n1 n2) with 
                | Some pds -> pds
                | None -> []              
            | None -> []
        | None -> []    
        
        
    let setRoutePatientDosages inds rt pds (dr : DoseRule) =
        let nInd = dr |> indxIndications inds
        
        match nInd with 
        | Some n1 -> 
            match 
                dr.IndicationsDosages.[n1]
                |> indxRoute rt with 
            | Some n2 ->
                dr |> Optic.set (routeDosPatientDosagesPrism n1 n2) pds
            | None -> dr
        | None -> dr
        
        
    let addPatient inds rt pat (dr : DoseRule) =
        let nInd = dr |> indxIndications inds
        
        let pds =
            dr
            |> getRoutePatientDosages inds rt
            |> List.append [ { Patient = pat; ShapeDosage = None; SubstanceDosages = [] } ]
        
        match nInd with 
        | Some n1 -> 
            match 
                dr.IndicationsDosages.[n1]
                |> indxRoute rt with 
            | Some n2 ->
                dr |> Optic.set (routeDosPatientDosagesPrism n1 n2) pds
            | None -> dr
        | None -> dr
    
    
    let indxPatient pat (rtd : RouteDosage) =
        rtd.PatientDosages
        |> List.tryFindIndex (fun rd -> rd.Patient = pat)
        
    
    let patDosPatientPrism n1 n2 n3 =
        (routeDosPatientDosagesPrism n1 n2) >?> List.pos_ n3 >?> PatientDosage.Patient_
    
    
    let getPatientAge inds rt pat dr = 
        match dr |> indxIndications inds with 
        | Some n1 ->
            match dr.IndicationsDosages.[n1] |> indxRoute rt with 
            | Some n2 ->
                match dr.IndicationsDosages.[n1].RouteDosages.[n2] |> indxPatient pat with 
                | Some n3 ->
                    dr |> Optic.get ((patDosPatientPrism n1 n2 n3) >?> Patient.Age_ )
                | None -> None
            | None -> None
        | None -> None
                

    let mdText = """
    Stofnaam: {generic}

    ATC code: {atc}

    Therapeutische groep: {thergroup} 

    Therapeutische subgroep: {thersub}

    Generiek groep: {gengroup}

    Generiek subgroep: {gensub}

    Doseringen:

    """

    let mdIndicationText = """
    Indicatie: {indication}
    """


    let mdRouteText = """
    Route: {route}
    """

    let print (dr : DoseRule) =
        mdText
        |> String.replace "{generic}" dr.Generic
        |> String.replace "{atc}" dr.ATC
        |> String.replace "{thergroup}" dr.ATCTherapyGroup
        |> String.replace "{thersub}" dr.ATCTherapySubGroup
        |> String.replace "{gengroup}" dr.GenericGroup
        |> String.replace "{gensub}" dr.GenericSubGroup
        |> (fun s ->
            dr.IndicationsDosages
            |> List.fold (fun acc id ->
                let i = 
                    id.Indications 
                    |> String.concat ", "

                id.RouteDosages
                |> List.fold (fun acc rd -> 
                      acc + (mdRouteText |> String.replace "{route}" rd.Route)
                ) (acc + (mdIndicationText |> String.replace "{indication}" i))
            ) s
        )



module Test =

    let test () =
        DoseRule.doseRule "paracetamol" "N02BE01" "Zenuwstelsel" "Analgetica" "Overige analgetica en antipyretica" "Aceetanilidederivaten" [] [] []
        |> DoseRule.addIndications ["Milde pijn en koorts"]
        |> DoseRule.addRoute ["Milde pijn en koorts"] "Oraal"
        |> DoseRule.addPatient ["Milde pijn en koorts"] "Oraal" (Patient.empty)
        |> DoseRule.print
        |> printfn "%s"
