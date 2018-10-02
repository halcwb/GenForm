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
            NormWeight : MinMax * WeightUnit
            // Normal limits adjusted by BSA
            NormBSA : MinMax * BSAUnit
            // Absolute limits
            Abs : MinMax
            // Absolute limits adjusted by weight
            AbsWeight : MinMax * WeightUnit
            // Absolute limits adjusted by BSA
            AbsBSA : MinMax * BSAUnit
        }
    and WeightUnit = string
    and BSAUnit = string

    let create norm normWght normBSA abs absWght absBSA =
        {
            Norm = norm
            NormWeight = normWght
            NormBSA = normBSA
            Abs = abs
            AbsWeight = absWght
            AbsBSA = absBSA
        }

    let emptyWeight = MinMax.empty, ""
    
    
    let emptyBSA = MinMax.empty, ""


    let empty = create MinMax.empty emptyWeight emptyBSA MinMax.empty emptyWeight emptyBSA


    type DoseRange with

        static member Norm_ :
            (DoseRange -> MinMax) * (MinMax -> DoseRange -> DoseRange) =
            (fun dr -> dr.Norm),
            (fun mm dr -> { dr with Norm = mm })

        static member NormWeight_ :
            (DoseRange -> (MinMax * WeightUnit)) * ((MinMax * WeightUnit) -> DoseRange -> DoseRange) =
            (fun dr -> dr.NormWeight),
            (fun mm dr -> { dr with NormWeight = mm })

        static member NormBSA_ :
            (DoseRange -> (MinMax * BSAUnit)) * ((MinMax * BSAUnit) -> DoseRange -> DoseRange) =
            (fun dr -> dr.NormBSA),
            (fun mm dr -> { dr with NormBSA = mm })

        static member Abs_ :
            (DoseRange -> MinMax) * (MinMax -> DoseRange -> DoseRange) =
            (fun dr -> dr.Abs),
            (fun mm dr -> { dr with Abs = mm })

        static member AbsWeight_ :
            (DoseRange -> (MinMax * WeightUnit)) * ((MinMax * WeightUnit) -> DoseRange -> DoseRange) =
            (fun dr -> dr.AbsWeight),
            (fun mm dr -> { dr with AbsWeight = mm })

        static member AbsBSA_ :
            (DoseRange -> (MinMax * BSAUnit)) * ((MinMax * BSAUnit) -> DoseRange -> DoseRange) =
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
        

    let minNormWeightLens = DoseRange.NormWeight_ >-> fst_ >-> MinMax.Min_


    let getMinNormWeight = Optic.get minNormWeightLens


    let setMinNormWeight = Optic.set minNormWeightLens


    let inclMinNormWeightLens =
        DoseRange.NormWeight_ >-> (MinMax.inclMinLens) 


    let getInclMinNormWeight = Optic.get inclMinNormWeightLens 


    let setInclMinNormWeight = Optic.set inclMinNormWeightLens