#I __SOURCE_DIRECTORY__

#load "./../../../.paket/load/netstandard2.0/main.group.fsx"

#r "./../../Informedica.GenUtils.Lib/bin/Debug/netstandard2.0/Informedica.GenUtils.Lib.dll"
#r "./../../Informedica.GenUnits.Lib/bin/Debug/netstandard2.0/Informedica.GenUnits.Lib.dll"
#r "./../../Informedica.GenProduct.Lib/bin/Debug/netstandard2.0/Informedica.GenProduct.Lib.dll"

#r "netstandard"

#time


open System

let pwd = Environment.GetEnvironmentVariable("HOME")
Environment.CurrentDirectory <- 
    if pwd |> String.IsNullOrWhiteSpace then 
        __SOURCE_DIRECTORY__ + "/../../../"

    else 
        pwd + "/Development/GenForm/" //__SOURCE_DIRECTORY__ + "/../../../"


open Informedica.GenUtils.Lib.BCL
open Informedica.GenUtils.Lib

open Aether
open Aether.Operators


module String =

    open System


    let equals s1 s2 = s1 = s2


    /// Apply `f` to string `s`
    let apply f (s: String) = f s
    

    /// Utility to enable type inference
    let get = apply id

    /// Get the length of s
    let length s = (s |> get).Length

    /// Get a substring starting at `start` with length `length`
    let substring start length (s: string) = s.Substring(start, length)


    /// Return the rest of a string as a string
    let restString s = substring 1 ((s |> length) - 1) s


    /// Check whether **s1** starts with
    /// **s2** using string comparison **eqs**
    let startsWithEqs eqs s2 s1 =
        if s2 |> length > (s1 |> length) then false
        else
            s1 |> substring 0 (s2 |> length) |> eqs s2


    /// Check whether **s1** starts with
    /// **s2** caps sensitive
    let startsWith = startsWithEqs equals

    /// Check whether **s1** ends with
    /// **s2** using string comparison **eqs**
    let endsWithEqs eqs s2 s1 =
        let len1 = s1 |> length
        let len2 = s2 |> length
        
        if len2 > len1 || len2 = 0 then false
        else
            s1 |> substring (len1 - len2) (len1 - (len1 - len2)) |> eqs s2


    /// Check whether **s1** ends with
    /// **s2** caps sensitive
    let endsWith = endsWithEqs equals


    let removeTrailing trail s =
        if s |> endsWith trail |> not then s
        else
            let len = trail |> length
            s |> substring 0 ((s |> length) - len)
            

    let removeTrailingEOL = removeTrailing "\n"


    let remBr (s: String) = 
        (String.regex "\[[^\]]*]").Replace(s, "")
   


module List =

    let toStr = List.toString

    let tryFindRest pred xs =
        let rec find x xs notx =
            match xs with
            | [] -> x, notx
            | h::tail ->
                if h |> pred then find (Some h) tail notx
                else find x tail ([h] |> List.append notx)

        find None xs []


    let toString xs =
        xs
        |> toStr
        |> String.replace "[" ""
        |> String.replace "]" ""
        |> String.replace ";" ","
    

    let prepend l1 l2 = List.append l2 l1



module Mapping =

    open System

    open Informedica.GenUtils.Lib
    open Informedica.GenUtils.Lib.BCL
    open Informedica.GenProduct.Lib

    [<Literal>]
    let unitPath = "/formulary/UnitMapping.csv"

    [<Literal>]
    let freqPath = "/formulary/FrequencyMapping.csv"

    type Mapping = FormMap | GStandMap | PedMap  | GenFormMap

    let map path m1 m2 s =
        let i1, i2 =
            match m1, m2 with
            | FormMap,    GStandMap  -> 0, 1
            | GStandMap,  FormMap    -> 1, 0
            | FormMap,    GenFormMap -> 0, 3
            | GStandMap,  GenFormMap -> 1, 3
            | GenFormMap, FormMap    -> 3, 0
            | GenFormMap, GStandMap  -> 1, 3
            | _ -> 0, 0

        let map =
            File.readAllLines path
            |> Array.skip 1
            |> Array.map (String.splitAt ';')
        if i1 = 0 && i2 = 0 || (i1 > map.Length || i2 > map.Length) then ""
        else
            map
            |> Array.filter (fun x -> x.[i1] |> String.equalsCapInsens s)
            |> Array.fold (fun acc xs ->  
                if acc = "" then xs.[i2]
                else acc + "||" + xs.[i2]
            ) ""


    let mapUnit = map (Environment.CurrentDirectory + "/" + FilePath.data + unitPath)

    let mapFreq = map (Environment.CurrentDirectory + "/" + FilePath.data + freqPath)



module ValueUnit =


    open Informedica.GenUtils.Lib
    open Informedica.GenUtils.Lib.BCL
    open Informedica.GenUnits.Lib

    open ValueUnit


    let unitToString u =
        u
        |> Units.toString Units.Dutch Units.Short
        |> String.remBr


    let unitFromString m u = 
            u
            |> Mapping.mapUnit m Mapping.GenFormMap
            |> Units.fromString

    let createUnit m v u =
        let s = 
            u
            |> Mapping.mapUnit m Mapping.GenFormMap
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


    let createFromFormul = createUnit Mapping.FormMap


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
        |> Mapping.mapUnit Mapping.GenFormMap Mapping.GStandMap


    let timeMinute = (fun n -> fromFloat n Units.Time.minute)


    let timeHour =  (fun n -> fromFloat n Units.Time.hour)


    let timeDay =  (fun n -> fromFloat n Units.Time.day)


    let timeWeek =  (fun n -> fromFloat n Units.Time.week)


    let ageInWk =  (fun n -> fromFloat n Units.Time.week)


    let ageInMo =  (fun n -> fromFloat n Units.Time.month)


    let ageInYr =  (fun n -> fromFloat n Units.Time.year)


    let weightInKg =  (fun n -> fromFloat n Units.Weight.kiloGram)


    let bsaInM2 =  (fun n -> fromFloat n Units.BSA.M2)


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



module MinMax =


    module ValueUnit = Informedica.GenUnits.Lib.ValueUnit

    type ValueUnit = ValueUnit.ValueUnit


    let (<?) = ValueUnit.st
    let (>?) = ValueUnit.gt
    let (<=?) = ValueUnit.ste
    let (>=?) = ValueUnit.gte


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
    

    let inline applyValue f1 f2 f3 f4 v1 v2 =
            match v1, v2 with
            | Inclusive vu1, Inclusive vu2 -> vu1 |> f1 <| vu2
            | Inclusive vu1, Exclusive vu2 -> vu1 |> f2 <| vu2
            | Exclusive vu1, Inclusive vu2 -> vu1 |> f3 <| vu2
            | Exclusive vu1, Exclusive vu2 -> vu1 |> f4 <| vu2


    let valueLT = applyValue (>?) (>=?) (>?) (>?) 


    let valueST = applyValue (<?) (<?) (<=?) (<?) 


    let valueLTE = applyValue (>=?) (>=?) (>=?) (>=?)


    let valueSTE = applyValue (<=?) (<=?) (<=?) (<=?) 


    let isValid ({ Min = min; Max = max }) =
        match min, max with
        | None, None -> true
        | Some _, None | None, Some _ -> true
        | Some v1, Some v2 ->
            applyValue (<=?) (<?) (<?) (<?) v1 v2


    let setMin min mm =
        let mm_ = { mm with Min = min }
        if mm_ |> isValid then mm_ else mm


    let setMax max mm =
        let mm_ = { mm with Max = max }
        if mm_ |> isValid then mm_ else mm


    let setMinCond cond min (mm : MinMax) =
        match mm.Min, mm.Max with
        | Some m, Some max ->
            if cond min m |> not then mm
            else
                mm
                |> setMin (Some min)
                |> setMax (Some max)
        | None, Some max -> 
            mm
            |> setMin (Some min)
            |> setMax (Some max)
        | None, None    -> 
            mm
            |> setMin (Some min)
        | Some m, None   -> 
            if cond min m |> not then mm
            else 
                mm
                |> setMin (Some min)


    let setMaxCond cond max (mm : MinMax) =
        match mm.Min, mm.Max with
        | Some min, Some m ->
            if cond max m |> not then mm
            else
                mm
                |> setMin (Some min)
                |> setMax (Some max)
        | Some min, None -> 
            mm
            |> setMin (Some min)
            |> setMax (Some max)
        | None, None  -> 
            mm
            |> setMax (Some max)
        | None, Some m -> 
            if cond max m |> not then mm
            else 
                mm
                |> setMax (Some max)
        

    let foldCond cond (mms : MinMax list) =
        let condMax m1 m2 = cond m2 m1
        mms |> List.fold (fun acc mm ->
            match mm.Min, mm.Max with
            | None, None         -> acc
            | Some min, None     -> setMinCond cond min acc
            | None, Some max     -> setMaxCond condMax max acc
            | Some min, Some max ->
                acc
                |> setMinCond cond min
                |> setMaxCond condMax max
        ) empty
     

    let foldMinimize = foldCond valueLT
     

    let foldMaximize = foldCond valueST


    let inRange v (mm : MinMax) =
        match mm.Min, mm.Max with
        | None, None -> true
        | Some v_, None -> valueLTE v v_
        | None, Some v_ -> valueSTE v v_
        | Some v1, Some v2 ->
            (valueLTE v v1) && (valueSTE v v2)



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

    
    type MinMax with
    
        static member Min_ :
            (MinMax -> Value Option) * (Value -> MinMax -> MinMax) =
            (fun mm -> mm.Min), 
            (fun v mm -> mm |> setMin (Some v))
    
        static member Max_ :
            (MinMax -> Value Option) * (Value -> MinMax -> MinMax) =
            (fun mm -> mm.Max), 
            (fun v mm -> mm |> setMax (Some v))


    
    module Optics =
    
    
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
            (fun vu mm -> 
                match vu with
                | Some vu_ -> mm |> setMin (vu_ |> inclusive)
                | None -> mm
            )
    
    
        let exclMinLens = 
            (fun mm -> 
                match mm |> getMin with 
                | Some min -> 
                    match min with 
                    | Exclusive v -> Some v 
                    | _ -> None 
                | None -> None),
            (fun vu mm -> 
                match vu with
                | Some vu_ -> mm |> setMin (vu_ |> exclusive)
                | None -> mm
            )
    
    
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
            (fun vu mm -> 
                match vu with
                | Some vu_ -> mm |> setMax (vu_ |> inclusive)
                | None -> mm
            )
    
    
        let exclMaxLens = 
            (fun mm -> 
                match mm |> getMax with 
                | Some max -> 
                    match max with 
                    | Exclusive v -> Some v 
                    | _ -> None 
                | None -> None),
            (fun vu mm -> 
                match vu with
                | Some vu_ -> mm |> setMax (vu_ |> exclusive)
                | None -> mm
            )
        

    let valueToString = function
        | Inclusive vu -> sprintf "incl %s" (vu |> ValueUnit.toStringPrec 2)
        | Exclusive vu -> sprintf "excl %s" (vu |> ValueUnit.toStringPrec 2)


    let toString { Min = min; Max = max } =
        let vuToStr = ValueUnit.toStringPrec 2

        let minToString min =
            match min with 
            | Inclusive vu ->
                vu |> vuToStr |> sprintf ">= %s"
            | Exclusive vu ->
                vu |> vuToStr |> sprintf "> %s"

        let maxToString min =
            match min with 
            | Inclusive vu ->
                vu |> vuToStr |> sprintf "<= %s"
            | Exclusive vu ->
                vu |> vuToStr |> sprintf "< %s"

        match min, max with
        | None, None -> ""
        | Some min_, Some max_ -> 
            sprintf "%s - %s" (min_ |> minToString) (max_ |> maxToString)
        | Some min_, None -> 
            (min_ |> minToString) 
        | None, Some max_ -> 
            (max_ |> maxToString)



module MinMaxTests =

    module MinMax = MinMax.Optics

    let v1, v2 = 
        ValueUnit.substanceInGStandUnit 10. "milligram" |> Option.get ,
        ValueUnit.substanceInGStandUnit 20. "milligram" |> Option.get
        
    let incl1, incl2 =
        v1 |> MinMax.inclusive, 
        v2 |> MinMax.inclusive

    let v3, v4 = 
        ValueUnit.substanceInGStandUnit 30. "milligram" |> Option.get ,
        ValueUnit.substanceInGStandUnit 40. "milligram" |> Option.get
        
    let incl3, incl4 =
        v3 |> MinMax.inclusive, 
        v4 |> MinMax.inclusive


    let toString () =
        MinMax.empty
        |> MinMax.setMin (ValueUnit.createFromGStand 1. "milligram" |> Option.get |> MinMax.Inclusive)
        |> MinMax.setMax (ValueUnit.createFromGStand 10. "milligram" |> Option.get |> MinMax.Inclusive)
        |> MinMax.toString


    let valueComp () =

        printfn "%A < %A = %A" incl1 incl2 (MinMax.valueST incl1 incl2)
        printfn "%A < %A = %A" incl1 incl1 (MinMax.valueST incl1 incl1)
        printfn "%A <= %A = %A" incl1 incl2 (MinMax.valueSTE incl1 incl2)
        printfn "%A <= %A = %A" incl1 incl1 (MinMax.valueSTE incl1 incl1)
        printfn ""
        printfn "%A > %A = %A" incl1 incl2 (MinMax.valueLT incl1 incl2)
        printfn "%A > %A = %A" incl1 incl1 (MinMax.valueLT incl1 incl1)
        printfn "%A >= %A = %A" incl1 incl2 (MinMax.valueLTE incl1 incl2)
        printfn "%A >= %A = %A" incl1 incl1 (MinMax.valueLTE incl1 incl1)


    // ToDo handle None cases correctly?
    let testFold () =
        let mms = 
            [
                MinMax.empty
                MinMax.empty |> MinMax.setMin incl1
                MinMax.empty |> MinMax.setMin incl2
                MinMax.empty |> MinMax.setMax incl3
                MinMax.empty |> MinMax.setMax incl4
                MinMax.empty |> MinMax.setMin incl1 |> MinMax.setMax incl3
                MinMax.empty |> MinMax.setMin incl2 |> MinMax.setMax incl3
                MinMax.empty |> MinMax.setMin incl3 |> MinMax.setMax incl3
                MinMax.empty |> MinMax.setMin incl4 |> MinMax.setMax incl4
            ]

        mms
        |> MinMax.foldMaximize
        |> MinMax.toString
        |> printfn "Maximized:\n%s"


        mms
        |> MinMax.foldMinimize
        |> MinMax.toString
        |> printfn "Minimized:\n%s"


    let inRange () =
        let mm1 = MinMax.empty
        let mm2 = 
            MinMax.empty
            |> MinMax.setMin incl1
        let mm3 = 
            MinMax.empty
            |> MinMax.setMax incl4
        let mm4 =
            MinMax.empty
            |> MinMax.setMin incl2
            |> MinMax.setMax incl3

        let test v mm =
            printfn "%A in range: %A = %A" (v |> MinMax.valueToString) (mm |> MinMax.toString) (MinMax.inRange v mm)


        [
            (incl1, mm1)
            (incl2, mm1)
            (incl3, mm1)
            (incl4, mm1)
            (incl1, mm2)
            (incl2, mm2)
            (incl3, mm2)
            (incl4, mm2)
            (incl1, mm3)
            (incl2, mm3)
            (incl3, mm3)
            (incl4, mm3)
            (incl1, mm4)
            (incl2, mm4)
            (incl3, mm4)
            (incl4, mm4)
        ]
        |> List.iter (fun (v, mm) -> test v mm)



module DoseRange =


    module ValueUnit = Informedica.GenUnits.Lib.ValueUnit


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
    and WeightUnit = ValueUnit.Unit
    and BSAUnit = ValueUnit.Unit

    let create norm normWght normBSA abs absWght absBSA =
        {
            Norm = norm
            NormWeight = normWght
            NormBSA = normBSA
            Abs = abs
            AbsWeight = absWght
            AbsBSA = absBSA
        }

    let emptyWeight = MinMax.empty, ValueUnit.NoUnit
    
    
    let emptyBSA = MinMax.empty, ValueUnit.NoUnit


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

    module Optics =
        
        module MinMax = MinMax.Optics


        let inclMinNormLens =
            DoseRange.Norm_ >-> MinMax.inclMinLens 


        let exclMinNormLens =
            DoseRange.Norm_ >-> MinMax.exclMinLens


        let inclMaxNormLens =
            DoseRange.Norm_ >-> (MinMax.inclMaxLens) 


        let exclMaxNormLens =
            DoseRange.Norm_ >-> (MinMax.exclMaxLens) 


        let normWeightUnitLens = DoseRange.NormWeight_ >-> snd_


        let inclMinNormWeightLens =
            DoseRange.NormWeight_ >-> fst_ >-> MinMax.inclMinLens


        let exclMinNormWeightLens =
            DoseRange.NormWeight_ >-> fst_ >-> MinMax.exclMinLens


        let inclMaxNormWeightLens =
            DoseRange.NormWeight_ >-> fst_ >-> MinMax.inclMaxLens


        let exclMaxNormWeightLens =
            DoseRange.NormWeight_ >-> fst_ >-> MinMax.exclMaxLens 


        let normBSAUnitLens = DoseRange.NormBSA_ >-> snd_


        let inclMinNormBSALens =
            DoseRange.NormBSA_ >-> fst_ >-> MinMax.inclMinLens


        let exclMinNormBSALens =
            DoseRange.NormBSA_ >-> fst_ >-> MinMax.exclMinLens


        let inclMaxNormBSALens =
            DoseRange.NormBSA_ >-> fst_ >-> MinMax.inclMaxLens


        let exclMaxNormBSALens =
            DoseRange.NormBSA_ >-> fst_ >-> MinMax.exclMaxLens


        let minAbsLens = DoseRange.Abs_ >-> MinMax.Min_


        let inclMinAbsLens =
            DoseRange.Abs_ >-> (MinMax.inclMinLens) 


        let exclMinAbsLens =
            DoseRange.Abs_ >-> (MinMax.exclMinLens) 


        let inclMaxAbsLens =
            DoseRange.Abs_ >-> (MinMax.inclMaxLens) 


        let exclMaxAbsLens =
            DoseRange.Abs_ >-> (MinMax.exclMaxLens) 


        let absWeightUnitLens = DoseRange.AbsWeight_ >-> snd_


        let inclMinAbsWeightLens =
            DoseRange.AbsWeight_ >-> fst_ >-> MinMax.inclMinLens


        let exclMinAbsWeightLens =
            DoseRange.AbsWeight_ >-> fst_ >-> MinMax.exclMinLens


        let inclMaxAbsWeightLens =
            DoseRange.AbsWeight_ >-> fst_ >-> MinMax.inclMaxLens


        let exclMaxAbsWeightLens =
            DoseRange.AbsWeight_ >-> fst_ >-> MinMax.exclMaxLens


        let absBSAUnitLens = DoseRange.AbsBSA_ >-> snd_


        let inclMinAbsBSALens =
            DoseRange.AbsBSA_ >-> fst_ >-> MinMax.inclMinLens


        let exclMinAbsBSALens =
            DoseRange.AbsBSA_ >-> fst_ >-> MinMax.exclMinLens


        let inclMaxAbsBSALens =
            DoseRange.AbsBSA_ >-> fst_ >-> MinMax.inclMaxLens


        let exclMaxAbsBSALens =
            DoseRange.AbsBSA_ >-> fst_ >-> MinMax.exclMaxLens


    let toString ({ Norm = norm; NormWeight = normwght; NormBSA = normbsa; Abs = abs; AbsWeight = abswght; AbsBSA = absbsa}) =
        let (>+) sl sr = 
            if sl |> String.isNullOrWhiteSpace then sr
            else
                let sr = if sr |> String.isNullOrWhiteSpace then sr else " of " + sr
                sl + sr
        
        let nw, nwu = normwght
        let nb, nbu = normbsa
        let aw, nau = abswght
        let ab, abu = absbsa

        let nwu = nwu |> ValueUnit.unitToString
        let nbu = nbu |> ValueUnit.unitToString
        let nau = nau |> ValueUnit.unitToString
        let abu = abu |> ValueUnit.unitToString
        
        let mmtoStr u mm = 
            mm 
            |> MinMax.toString 
            |> (fun s -> 
                if s |> String.isNullOrWhiteSpace || u |> String.isNullOrWhiteSpace then s 
                else s + "/" + u
            )
            
        norm 
        |> MinMax.toString
        >+ (nw |> mmtoStr nwu)
        >+ (nb |> mmtoStr nbu)
        |> (fun s -> 
            if s |> String.isNullOrWhiteSpace then s
            else "Normaal dosering: " + s
        )
        |> (fun sn ->
            let sa =
                abs |> MinMax.toString
                >+ (aw |> mmtoStr nau)
                >+ (ab |> mmtoStr abu)
            if sa |> String.isNullOrWhiteSpace then sn
            else 
                let sn = if sn |> String.isNullOrWhiteSpace then sn else sn + "\n"
                sn + "Maximale dosering: " + sa
        )
        


module DoseRangeTests =

    module DoseRange = DoseRange.Optics

    let setMaxNormDose = Optic.set DoseRange.inclMaxNormLens
    let setMaxAbsDose = Optic.set DoseRange.inclMaxAbsLens

    let toString () =
        DoseRange.empty
        |> setMaxNormDose (ValueUnit.createFromGStand 10. "milligram")
        |> setMaxAbsDose (ValueUnit.createFromGStand 100. "milligram")
        |> DoseRange.toString


    
module Dosage =

    module ValueUnit = Informedica.GenUnits.Lib.ValueUnit


    type DoseRange = DoseRange.DoseRange
    type ValueUnit = ValueUnit.ValueUnit
    type Unit = ValueUnit.Unit

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
            TotalDosage : DoseRange * TimeUnit
            /// Allowed frequencies
            Frequencies : Frequency
        }
    and Frequency = 
        {
            Frequencies : Frequencies
            TimeUnit : TimeUnit
            MinimalInterval : ValueUnit Option
        }
    and Frequencies = int list
    and TimeUnit = Unit
    and RateUnit = Unit


    let create nm start single rate total freqs =
        {
            Name = nm
            StartDosage = start
            SingleDosage = single
            RateDosage = rate
            TotalDosage = total
            Frequencies = freqs
        }

    let emptyFrequencies = { Frequencies = []; TimeUnit = ValueUnit.NoUnit; MinimalInterval = None }


    let empty = create "" DoseRange.empty DoseRange.empty (DoseRange.empty, ValueUnit.NoUnit) (DoseRange.empty, ValueUnit.NoUnit) emptyFrequencies


    type Frequency with

        static member Frequencies_ :
            (Frequency -> Frequencies) * (Frequencies -> Frequency -> Frequency) =
            (fun fr -> fr.Frequencies) ,
            (fun frs fr -> { fr with Frequencies = frs })

        static member TimeUnit_ :
            (Frequency -> TimeUnit) * (TimeUnit -> Frequency -> Frequency) =
            (fun fr -> fr.TimeUnit) ,
            (fun tu fr -> { fr with TimeUnit = tu })

        static member MinimalInterval_ :
            (Frequency -> ValueUnit Option) * (ValueUnit Option -> Frequency -> Frequency) =
            (fun fr -> fr.MinimalInterval) ,
            (fun mi fr -> { fr with MinimalInterval = mi })


    type Dosage with
        
        static member Name_ :
            (Dosage -> string) * (string -> Dosage -> Dosage) =
            (fun d -> d.Name),
            (fun s d -> { d with Name = s })

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
            (Dosage -> (DoseRange * TimeUnit)) * ((DoseRange * TimeUnit) -> Dosage -> Dosage) =
            (fun d -> d.TotalDosage),
            (fun dt d -> { d with TotalDosage = dt })
            
        static member Frequencies_ :
            (Dosage -> Frequency) * (Frequency -> Dosage -> Dosage) =
            (fun d -> d.Frequencies) ,
            (fun frqs d -> { d with Frequencies = frqs })        

    
    module Optics =

        module DoseRange = DoseRange.Optics


        let freqsFrequencyLens =
            Dosage.Frequencies_ >-> Frequency.Frequencies_

        
        let timeUnitFrequencyLens =
            Dosage.Frequencies_ >-> Frequency.TimeUnit_


        let minIntervalValueFrequencyLens =
            Dosage.Frequencies_ >-> Frequency.MinimalInterval_


        let inclMinNormStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.inclMinNormLens


        let exclMinNormStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.exclMinNormLens


        let inclMaxNormStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.inclMaxNormLens


        let exclMaxNormStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.exclMaxNormLens

        
        let normWeightUnitStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.normWeightUnitLens


        let inclMinNormWeightStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.inclMinNormWeightLens


        let exclMinNormWeightStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.exclMinNormWeightLens


        let inclMaxNormWeightStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.inclMaxNormWeightLens


        let exclMaxNormWeightStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.exclMaxNormWeightLens


        let normBSAUnitStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.normBSAUnitLens


        let inclMinNormBSAStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.inclMinNormBSALens


        let exclMinNormBSAStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.exclMinNormBSALens


        let inclMaxNormBSAStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.inclMaxNormBSALens


        let exclMaxNormBSAStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.exclMaxNormBSALens


        let inclMinNormSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.inclMinNormLens


        let exclMinNormSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.exclMinNormLens


        let inclMaxNormSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.inclMaxNormLens


        let exclMaxNormSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.exclMaxNormLens

        
        let normWeightUnitSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.normWeightUnitLens


        let inclMinNormWeightSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.inclMinNormWeightLens


        let exclMinNormWeightSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.exclMinNormWeightLens


        let inclMaxNormWeightSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.inclMaxNormWeightLens


        let exclMaxNormWeightSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.exclMaxNormWeightLens

        
        let normBSAUnitSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.normBSAUnitLens


        let inclMinNormBSASingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.inclMinNormBSALens


        let exclMinNormBSASingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.exclMinNormBSALens


        let inclMaxNormBSASingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.inclMaxNormBSALens


        let exclMaxNormBSASingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.exclMaxNormBSALens


        let rateUnitRateDosagePrism =
            Dosage.RateDosage_ >-> snd_


        let normWeightUnitRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.normWeightUnitLens


        let inclMinNormRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMinNormLens


        let exclMinNormRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMinNormLens


        let inclMaxNormRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMaxNormLens


        let exclMaxNormRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMaxNormLens


        let inclMinNormWeightRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMinNormWeightLens


        let exclMinNormWeightRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMinNormWeightLens


        let inclMaxNormWeightRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMaxNormWeightLens


        let exclMaxNormWeightRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMaxNormWeightLens


        let normBSAUnitRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.normBSAUnitLens


        let inclMinNormBSARateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMinNormBSALens


        let exclMinNormBSARateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMinNormBSALens


        let inclMaxNormBSARateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMaxNormBSALens


        let exclMaxNormBSARateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMaxNormBSALens


        let timeUnitTotalDosagePrism =
            Dosage.TotalDosage_ >-> snd_


        let normWeightUnitTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.normWeightUnitLens


        let inclMinNormTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMinNormLens


        let exclMinNormTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMinNormLens


        let inclMaxNormTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMaxNormLens


        let exclMaxNormTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMaxNormLens


        let inclMinNormWeightTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMinNormWeightLens


        let exclMinNormWeightTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMinNormWeightLens


        let inclMaxNormWeightTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMaxNormWeightLens


        let exclMaxNormWeightTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMaxNormWeightLens


        let normBSAUnitTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.normBSAUnitLens


        let inclMinNormBSATotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMinNormBSALens


        let exclMinNormBSATotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMinNormBSALens


        let inclMaxNormBSATotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMaxNormBSALens


        let exclMaxNormBSATotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMaxNormBSALens


        let inclMinAbsStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.inclMinAbsLens


        let exclMinAbsStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.exclMinAbsLens


        let inclMaxAbsStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.inclMaxAbsLens


        let exclMaxAbsStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.exclMaxAbsLens


        let absWeightUnitStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.absWeightUnitLens


        let inclMinAbsWeightStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.inclMinAbsWeightLens


        let exclMinAbsWeightStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.exclMinAbsWeightLens


        let inclMaxAbsWeightStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.inclMaxAbsWeightLens


        let exclMaxAbsWeightStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.exclMaxAbsWeightLens


        let absBSAUnitStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.absBSAUnitLens


        let inclMinAbsBSAStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.inclMinAbsBSALens


        let exclMinAbsBSAStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.exclMinAbsBSALens


        let inclMaxAbsBSAStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.inclMaxAbsBSALens


        let exclMaxAbsBSAStartDosagePrism =
            Dosage.StartDosage_ >-> DoseRange.exclMaxAbsBSALens
    

        let inclMinAbsSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.inclMinAbsLens


        let exclMinAbsSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.exclMinAbsLens


        let inclMaxAbsSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.inclMaxAbsLens


        let exclMaxAbsSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.exclMaxAbsLens


        let absWeightUnitSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.absWeightUnitLens


        let inclMinAbsWeightSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.inclMinAbsWeightLens


        let exclMinAbsWeightSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.exclMinAbsWeightLens


        let inclMaxAbsWeightSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.inclMaxAbsWeightLens


        let exclMaxAbsWeightSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.exclMaxAbsWeightLens


        let absBSAUnitSingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.absBSAUnitLens


        let inclMinAbsBSASingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.inclMinAbsBSALens


        let exclMinAbsBSASingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.exclMinAbsBSALens


        let inclMaxAbsBSASingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.inclMaxAbsBSALens


        let exclMaxAbsBSASingleDosagePrism =
            Dosage.SingleDosage_ >-> DoseRange.exclMaxAbsBSALens


        let inclMinAbsRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMinAbsLens


        let exclMinAbsRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMinAbsLens


        let inclMaxAbsRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMaxAbsLens


        let exclMaxAbsRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMaxAbsLens


        let absWeightUnitRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.absWeightUnitLens


        let inclMinAbsWeightRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMinAbsWeightLens


        let exclMinAbsWeightRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMinAbsWeightLens


        let inclMaxAbsWeightRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMaxAbsWeightLens


        let exclMaxAbsWeightRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMaxAbsWeightLens


        let absBSAUnitRateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.absBSAUnitLens


        let inclMinAbsBSARateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMinAbsBSALens


        let exclMinAbsBSARateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMinAbsBSALens


        let inclMaxAbsBSARateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.inclMaxAbsBSALens


        let exclMaxAbsBSARateDosagePrism =
            Dosage.RateDosage_ >-> fst_ >-> DoseRange.exclMaxAbsBSALens


        let inclMinAbsTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMinAbsLens


        let exclMinAbsTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMinAbsLens


        let inclMaxAbsTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMaxAbsLens


        let exclMaxAbsTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMaxAbsLens


        let absWeightUnitTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.absWeightUnitLens


        let inclMinAbsWeightTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMinAbsWeightLens


        let exclMinAbsWeightTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMinAbsWeightLens


        let inclMaxAbsWeightTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMaxAbsWeightLens


        let exclMaxAbsWeightTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMaxAbsWeightLens


        let absBSAUnitTotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.absBSAUnitLens


        let inclMinAbsBSATotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMinAbsBSALens


        let exclMinAbsBSATotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMinAbsBSALens


        let inclMaxAbsBSATotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.inclMaxAbsBSALens


        let exclMaxAbsBSATotalDosagePrism =
            Dosage.TotalDosage_ >-> fst_ >-> DoseRange.exclMaxAbsBSALens



    let toString ({ Name = n; StartDosage = start; SingleDosage = single; RateDosage = rate; TotalDosage = total; Frequencies = freqs }) =
        let vuToStr = ValueUnit.toStringPrec 2
        
        let (>+) sl sr = 
            let l, s, u = sr
            let u = if u |> String.isNullOrWhiteSpace then u else "/" + u

            if s |> String.isNullOrWhiteSpace then sl
            else 
                (if sl |> String.isNullOrWhiteSpace then sl else sl + "\n") + l + "\n" + s + u + "\n"
            
        let rt, _ = rate
        let tt, _ = total

        let fu = freqs.TimeUnit |> ValueUnit.unitToString

        n
        >+ ("Start dosering: ", start |> DoseRange.toString, "")
        >+ ("Keer dosering: ", single |> DoseRange.toString, "")
        >+ ("Continue dosering: ", rt |> DoseRange.toString, "")
        >+ ("Totaal dosering: ",   tt |> DoseRange.toString, "")
        |> (fun s -> 
            if freqs.Frequencies |> List.isEmpty || 
               fu |> String.isNullOrWhiteSpace then s
            else
                sprintf "%s\nToegestane frequenties: %s keer per %s" s (freqs.Frequencies |> List.toString) fu
                |> (fun s ->
                    match freqs.MinimalInterval with
                    | Some mi ->
                        s + "\n" + (sprintf "Minimaal interval: %s" (mi |> vuToStr))
                    | None -> s

                )
        )
        |> String.removeTrailingEOL



module DosageTests =
    
    module Dosage = Dosage.Optics

    let setNormMinStartDose = Optic.set Dosage.inclMinNormStartDosagePrism
    let setAbsMaxStartDose = Optic.set Dosage.inclMaxAbsStartDosagePrism

    let setNormMinSingleDose = Optic.set Dosage.inclMinNormSingleDosagePrism
    let setAbsMaxSingleDose = Optic.set Dosage.inclMaxAbsSingleDosagePrism

    let toString () =
        Dosage.empty
        |> setNormMinStartDose (ValueUnit.createFromGStand 10. "milligram")
        |> setAbsMaxStartDose (ValueUnit.createFromGStand 1. "gram")
        |> setNormMinSingleDose (ValueUnit.createFromGStand 10. "milligram")
        |> setAbsMaxSingleDose (ValueUnit.createFromGStand 1. "gram")
        |> Dosage.toString 



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
            
            if s |> String.isNullOrWhiteSpace then sl
            else sl + " " + l + s
        
        "Patient: "
        >+ ("Zwangerschapsduur: ", ga |> MinMax.toString)
        >+ ("Leeftijd: ", age |> MinMax.toString)
        >+ ("Gewicht: ", wght |> MinMax.toString)
        >+ ("BSA: ", bsa |> MinMax.toString)
        >+ ("Geslacht: ", gen |> genderToString)
        |> String.removeTrailingEOL



module PatientTests =

    module Patient = Patient.Optics

    
    let toString () =
        Patient.empty
        |> Patient.setInclMinGestAge (28.  |> ValueUnit.ageInWk)
        |> Patient.setExclMaxGestAge (33.  |> ValueUnit.ageInWk)
        |> Patient.setInclMinWeight (0.15  |> ValueUnit.weightInKg)
        |> Patient.setInclMaxWeight (4.00  |> ValueUnit.weightInKg)
        |> Patient.toString



module DoseRule =


    type Dosage = Dosage.Dosage
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
            // The doserules per indication(-s)
            IndicationsDosages : IndicationDosage list
        }
    and IndicationDosage =
        {
            // The indication(-s) the dose rule applies to
            Indications : string list
            // The dosage rules per administration route
            RouteDosages : RouteDosage list
        }
    and RouteDosage =
        {
            // Administration route
            Route : string
            // The dosage rules per shape
            ShapeDosages : ShapeDosage list
        } 
    and ShapeDosage =
        {
            // Name of the shape the doserule applies to
            Shape : String list
            // TradeProducts the doserule applies to
            TradeProducts : TradeProduct list
            // GenericProducts the doserule applies to
            GenericProducts : GenericProduct list
            // Patients to wich the doserule applies to
            PatientDosages : PatientDosage list
        }
    and PatientDosage =
        {
            // The patient group the doserules applies
            Patient : Patient
            // List of shapes that have a dosage
            ShapeDosage : Dosage
            // List of substances that have a dosage
            SubstanceDosages : Dosage list            
        }
    and TradeProduct = string
    and GenericProduct = string


    let create gen atc thg sub ggp gsg idl =
        {   
            Generic = gen
            ATC = atc
            ATCTherapyGroup = thg
            ATCTherapySubGroup = sub
            GenericGroup = ggp
            GenericSubGroup = gsg
            IndicationsDosages = idl
        }


    let createIndicationDosage inds =
        { Indications = inds; RouteDosages = [] }
            

    let createRouteDosage rt =
        if rt |> String.isNullOrWhiteSpace then None
        else 
            { Route = rt; ShapeDosages = [] } 
            |> Some


    let createShapeDosage shp gps tps =        
        if shp |> List.exists String.isNullOrWhiteSpace then None
        else 
            { Shape = shp; GenericProducts = gps; TradeProducts = tps; PatientDosages = [] }
            |> Some


    let createDosage n = Dosage.empty |> (Optic.set Dosage.Name_) n


    let createPatientDosage pat =
        { Patient = pat; ShapeDosage = Dosage.empty; SubstanceDosages = [] }


    let createSubstanceDosage sn =
        if sn |> String.isNullOrWhiteSpace then None
        else sn |> createDosage |> Some
            
        
    let indxIndications inds (dr : DoseRule) =
        dr.IndicationsDosages
        |> List.tryFindIndex (fun id -> id.Indications = inds)       
        

    let indxRoute inds rt dr =
        dr
        |> indxIndications inds
        |> Option.bind (fun ni -> 
            match
                dr.IndicationsDosages.[ni].RouteDosages 
                |> List.tryFindIndex (fun rd -> rd.Route = rt) with
            | None -> None
            | Some nr -> (ni, nr) |> Some
        )


    let indxShape inds rt shp dr =
        match dr |> indxRoute inds rt with
        | Some (ni, nr) ->
            match dr.IndicationsDosages.[ni].RouteDosages.[nr].ShapeDosages
                  |> List.tryFindIndex (fun sd -> sd.Shape = shp) with
            | Some ns -> (ni, nr, ns) |> Some
            | None -> None
        | None -> None


    let indxPatient inds rt shp pat dr =
        match dr |> indxShape inds rt shp with
        | Some (ni, nr, ns) ->
            match
                dr.IndicationsDosages.[ni].RouteDosages.[nr].ShapeDosages.[ns].PatientDosages
                |> List.tryFindIndex (fun rd -> rd.Patient = pat) with
            | Some np ->  (ni, nr, ns, np) |> Some
            | None -> None
        | None -> None


    let indxSubstance inds rt shp pat n dr =
        match dr |> indxPatient inds rt shp pat with
        | Some (ni, nr, ns, np) ->
            match dr.IndicationsDosages.[ni].RouteDosages.[nr].ShapeDosages.[np].PatientDosages.[ns].SubstanceDosages
                  |> List.tryFindIndex (fun sd -> sd.Name = n) with
            | Some n -> (ni, nr, np, ns, n) |> Some
            | None -> None
        | None -> None


    let addIndications inds (dr : DoseRule) =
        let indd = createIndicationDosage inds

        match dr |> indxIndications inds with
        | Some _ -> dr
        | None ->
            {
                dr with 
                    IndicationsDosages =
                        dr.IndicationsDosages
                        |> List.prepend [ indd ]
            }

 
    type PatientDosage with
    
        static member Patient_ :
            (PatientDosage -> Patient) * (Patient -> PatientDosage -> PatientDosage) =
            (fun pd -> pd.Patient) ,
            (fun pat pd -> { pd with Patient = pat })
    
        static member ShapeDosage_ :
            (PatientDosage -> Dosage) * (Dosage -> PatientDosage -> PatientDosage) =
            (fun pd -> pd.ShapeDosage) ,
            (fun sd pd -> { pd with ShapeDosage = sd })
    
        static member SubstanceDosages_ :
            (PatientDosage -> Dosage list) * (Dosage list -> PatientDosage -> PatientDosage) =
            (fun sd -> sd.SubstanceDosages) ,
            (fun d sd -> { sd with SubstanceDosages = d })


    type ShapeDosage with
    
        static member Shape_ :
            (ShapeDosage -> string list) * (string list -> ShapeDosage -> ShapeDosage) =
            (fun rd -> rd.Shape) ,
            (fun s rd -> { rd with Shape = s })

        static member TradeProducts_ :
            (ShapeDosage -> TradeProduct list) * (TradeProduct list -> ShapeDosage -> ShapeDosage) =
            (fun sd -> sd.TradeProducts) ,
            (fun tps sd -> { sd with TradeProducts = tps })

        static member GenericProducts_ :
            (ShapeDosage -> GenericProduct list) * (GenericProduct list -> ShapeDosage -> ShapeDosage) =
            (fun sd -> sd.GenericProducts) ,
            (fun tps sd -> { sd with GenericProducts = tps })
            
        static member PatientDosages_ :
            (ShapeDosage -> PatientDosage list) * (PatientDosage list -> ShapeDosage -> ShapeDosage) =
            (fun rd -> rd.PatientDosages) ,
            (fun pdl rd -> { rd with PatientDosages = pdl })            
 
        
    type RouteDosage with
    
        static member Route_ :
            (RouteDosage -> string) * (string -> RouteDosage -> RouteDosage) =
            (fun rd -> rd.Route) ,
            (fun s rd -> { rd with Route = s })
            
        static member ShapeDosages_ :
            (RouteDosage -> ShapeDosage list) * (ShapeDosage list -> RouteDosage -> RouteDosage) =
            (fun rd -> rd.ShapeDosages) ,
            (fun pdl rd -> { rd with ShapeDosages = pdl })            
            
        
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


    module Optics =
        
        module Patient = Patient.Optics
        module Dosage = Dosage.Optics
    
    
        let indDosDosagesLens n =
            DoseRule.IndicationDosages_ >-> List.pos_ n >?> IndicationDosage.RouteDosages_


        let getRouteDosages indd dr =
            match dr |> indxIndications indd with
            | Some n ->
                match dr |> Optic.get (indDosDosagesLens n) with
                | Some rtds -> rtds
                | None -> []
            | None -> []
        
    
        let addRoute inds rt dr =
            match rt |> createRouteDosage with
            | None -> dr
            | Some rtd ->
                match 
                    dr |> indxIndications inds with 
                    | Some n -> 
                        match dr |> indxRoute inds rt with
                        | Some _ -> dr
                        | None -> 
                            dr 
                            |> Optic.set (indDosDosagesLens n) (dr |> getRouteDosages inds |> List.prepend [rtd])
                    | None -> dr


        let shapeDosagesPrism n1 n2 =
            indDosDosagesLens n1 >?> List.pos_ n2 >?> RouteDosage.ShapeDosages_
        
        
        let getShapeDosages inds rt dr =

            match dr |> indxRoute inds rt with 
            | Some (ni, nr) -> 
                match dr |> Optic.get (shapeDosagesPrism ni nr) with 
                | Some pds -> pds
                | None -> []              
            | None -> []
        
        
        let setShapeDosages inds rt pds dr =

            match dr |> indxRoute inds rt with 
            | Some (ni, nr) -> 
                dr 
                |> Optic.set (shapeDosagesPrism ni nr) pds
            | None -> dr
        
        
        let addShape inds rt shp dr =
            match createShapeDosage shp [] [] with
            | None -> dr
            | Some shpd ->

                match dr |> indxShape inds rt shp with
                | Some _ -> dr
                | None ->
                    let pds =
                        dr
                        |> getShapeDosages inds rt
                        |> List.prepend [ shpd ]
        
                    dr 
                    |> setShapeDosages inds rt pds


        let shapeDosagePrism n1 n2 n3 =
            shapeDosagesPrism n1 n2 >?> List.pos_ n3 
        
    
        let patientDosagesPrism n1 n2 n3 =
            shapeDosagePrism n1 n2 n3 >?> ShapeDosage.PatientDosages_

        
        let getPatientDosages inds rt shp dr =
            match dr |> indxShape inds rt shp with 
            | Some (ni, nr, ns) -> 
                match dr 
                      |> Optic.get (patientDosagesPrism ni nr ns) with 
                | Some sds -> sds
                | None -> []
            | None -> []    
        
        
        let setPatientDosages inds rt shp pds dr =        
            match dr |> indxShape inds rt shp with 
            | Some (ni, nr, ns) -> 
                        dr 
                        |> Optic.set (patientDosagesPrism ni nr ns) pds 
            | None -> dr
        
        
        let addPatient inds rt shp pat dr =
            match dr |> indxPatient inds rt shp pat with
            | Some _ -> dr
            | None ->
                let pds =
                    dr
                    |> getPatientDosages inds rt shp
                    |> List.prepend [ createPatientDosage pat ]
        
                dr
                |> setPatientDosages inds rt shp pds
    

        let patientDosagePrism n1 n2 n3 n4 =
            patientDosagesPrism n1 n2 n3 >?> List.pos_ n4


        let patientPrism n1 n2 n3 n4 =
            patientDosagePrism n1 n2 n3 n4 >?> PatientDosage.Patient_

    
        let private patientGetter prism inds rt shp pat dr = 
            match dr |> indxPatient inds rt shp pat with
            | Some (ni, nr, ns, np) ->
                dr |> Optic.get ((patientPrism ni nr ns np) >?> prism)
            | None -> None
    
    
        let private patientSetter prism inds rt shp vu pat dr = 
            match dr |> indxPatient inds rt shp pat with 
            | Some (ni, nr, ns, np) ->
                let pat = 
                    pat |> prism vu
                dr |> Optic.set ((patientPrism ni nr ns np)) pat, pat
            | None -> dr, pat

 
        let getPatientInclMinGestAge = patientGetter Patient.inclMinGestAge
                  
    
        let setPatientInclMinGestAge = patientSetter Patient.setInclMinGestAge 

 
        let getPatientExclMinGestAge = patientGetter Patient.exclMinGestAge
                  
    
        let setPatientExclMinGestAge = patientSetter Patient.setExclMinGestAge 
 
 
        let getPatientInclMaxGestAge = patientGetter Patient.inclMaxGestAge
                  
    
        let setPatientInclMaxGestAge = patientSetter Patient.setInclMaxGestAge 

 
        let getPatientExclMaxGestAge = patientGetter Patient.exclMaxGestAge
                  
    
        let setPatientExclMaxGestAge = patientSetter Patient.setExclMaxGestAge 

 
        let getPatientInclMinAge = patientGetter Patient.inclMinAge
                  
    
        let setPatientInclMinAge = patientSetter Patient.setInclMinAge 

 
        let getPatientExclMinAge = patientGetter Patient.exclMinAge
                  
    
        let setPatientExclMinAge = patientSetter Patient.setExclMinAge 
 
 
        let getPatientInclMaxAge = patientGetter Patient.inclMaxAge
                  
    
        let setPatientInclMaxAge = patientSetter Patient.setInclMaxAge 

 
        let getPatientExclMaxAge = patientGetter Patient.exclMaxAge
                  
    
        let setPatientExclMaxAge = patientSetter Patient.setExclMaxAge 

 
        let getPatientInclMinWeight = patientGetter Patient.inclMinWeight
                  
    
        let setPatientInclMinWeight = patientSetter Patient.setInclMinWeight 

 
        let getPatientExclMinWeight = patientGetter Patient.exclMinWeight
                  
    
        let setPatientExclMinWeight = patientSetter Patient.setExclMinWeight 
 
 
        let getPatientInclMaxWeight = patientGetter Patient.inclMaxWeight
                  
    
        let setPatientInclMaxWeight = patientSetter Patient.setInclMaxWeight 

 
        let getPatientExclMaxWeight = patientGetter Patient.exclMaxWeight
                  
    
        let setPatientExclMaxWeight = patientSetter Patient.setExclMaxWeight 

 
        let getPatientInclMinBSA = patientGetter Patient.inclMinBSA
                  
    
        let setPatientInclMinBSA = patientSetter Patient.setInclMinBSA 

 
        let getPatientExclMinBSA = patientGetter Patient.exclMinBSA
                  
    
        let setPatientExclMinBSA = patientSetter Patient.setExclMinBSA 
 
 
        let getPatientInclMaxBSA = patientGetter Patient.inclMaxBSA
                  
    
        let setPatientInclMaxBSA = patientSetter Patient.setInclMaxBSA 

 
        let getPatientExclMaxBSA = patientGetter Patient.exclMaxBSA
                  
    
        let setPatientExclMaxBSA = patientSetter Patient.setExclMaxBSA 

        // TODO : add gender setting 
    
        
        let patientShapeDosagePrism n1 n2 n3 n4 =
            patientDosagePrism n1 n2 n3 n4 >?> PatientDosage.ShapeDosage_
 
        
        let inline private shapeDosageGetter prism inds rt shp pat dr = 
            match dr |> indxPatient inds rt shp pat with
            | Some (ni, nr, ns, np) ->
                dr |> Optic.get ((patientShapeDosagePrism ni nr ns np) >?> prism)
            | None -> None
    
    
        let inline private shapeDosageSetter prism inds rt shp vu pat dr = 
            match dr |> indxPatient inds rt shp pat with 
            | Some (ni, nr, ns, np) ->  
                dr |> Optic.set ((patientShapeDosagePrism ni nr ns np) >?> prism) vu
            | None -> dr


        let getFrequenciesShapeDosage = shapeDosageGetter Dosage.Frequencies_


        let setFrequenciesShapeDosage = shapeDosageSetter Dosage.Frequencies_

        
        let getInclMinNormStartShapeDosage = shapeDosageGetter Dosage.inclMinNormStartDosagePrism


        let setInclMinNormStartShapeDosage = shapeDosageSetter Dosage.inclMinNormStartDosagePrism
        
        
        let getExclMinNormStartShapeDosage = shapeDosageGetter Dosage.exclMinNormStartDosagePrism


        let setExclMinNormStartShapeDosage = shapeDosageSetter Dosage.exclMinNormStartDosagePrism

        
        let getInclMaxNormStartShapeDosage = shapeDosageGetter Dosage.inclMaxNormStartDosagePrism


        let setInclMaxNormStartShapeDosage = shapeDosageSetter Dosage.inclMaxNormStartDosagePrism
        
        
        let getExclMaxNormStartShapeDosage = shapeDosageGetter Dosage.exclMaxNormStartDosagePrism


        let setExclMaxNormStartShapeDosage = shapeDosageSetter Dosage.exclMaxNormStartDosagePrism


        let getInclMinNormWeightStartShapeDosage = shapeDosageGetter Dosage.inclMinNormWeightStartDosagePrism


        let setInclMinNormWeightStartShapeDosage = shapeDosageSetter Dosage.inclMinNormWeightStartDosagePrism
        
        
        let getExclMinNormWeightStartShapeDosage = shapeDosageGetter Dosage.exclMinNormWeightStartDosagePrism


        let setExclMinNormWeightStartShapeDosage = shapeDosageSetter Dosage.exclMinNormWeightStartDosagePrism

        
        let getInclMaxNormWeightStartShapeDosage = shapeDosageGetter Dosage.inclMaxNormWeightStartDosagePrism


        let setInclMaxNormWeightStartShapeDosage = shapeDosageSetter Dosage.inclMaxNormWeightStartDosagePrism
        
        
        let getExclMaxNormWeightStartShapeDosage = shapeDosageGetter Dosage.exclMaxNormWeightStartDosagePrism


        let setExclMaxNormWeightStartShapeDosage = shapeDosageSetter Dosage.exclMaxNormWeightStartDosagePrism


        let getInclMinNormBSAStartShapeDosage = shapeDosageGetter Dosage.inclMinNormBSAStartDosagePrism


        let setInclMinNormBSAStartShapeDosage = shapeDosageSetter Dosage.inclMinNormBSAStartDosagePrism
        
        
        let getExclMinNormBSAStartShapeDosage = shapeDosageGetter Dosage.exclMinNormBSAStartDosagePrism


        let setExclMinNormBSAStartShapeDosage = shapeDosageSetter Dosage.exclMinNormBSAStartDosagePrism

        
        let getInclMaxNormBSAStartShapeDosage = shapeDosageGetter Dosage.inclMaxNormBSAStartDosagePrism


        let setInclMaxNormBSAStartShapeDosage = shapeDosageSetter Dosage.inclMaxNormBSAStartDosagePrism
        
        
        let getExclMaxNormBSAStartShapeDosage = shapeDosageGetter Dosage.exclMaxNormBSAStartDosagePrism


        let setExclMaxNormBSAStartShapeDosage = shapeDosageSetter Dosage.exclMaxNormBSAStartDosagePrism


        let getInclMinAbsStartShapeDosage = shapeDosageGetter Dosage.inclMinAbsStartDosagePrism


        let setInclMinAbsStartShapeDosage = shapeDosageSetter Dosage.inclMinAbsStartDosagePrism
        
        
        let getExclMinAbsStartShapeDosage = shapeDosageGetter Dosage.exclMinAbsStartDosagePrism


        let setExclMinAbsStartShapeDosage = shapeDosageSetter Dosage.exclMinAbsStartDosagePrism

        
        let getInclMaxAbsStartShapeDosage = shapeDosageGetter Dosage.inclMaxAbsStartDosagePrism


        let setInclMaxAbsStartShapeDosage = shapeDosageSetter Dosage.inclMaxAbsStartDosagePrism
        
        
        let getExclMaxAbsStartShapeDosage = shapeDosageGetter Dosage.exclMaxAbsStartDosagePrism


        let setExclMaxAbsStartShapeDosage = shapeDosageSetter Dosage.exclMaxAbsStartDosagePrism


        let getInclMinAbsWeightStartShapeDosage = shapeDosageGetter Dosage.inclMinAbsWeightStartDosagePrism


        let setInclMinAbsWeightStartShapeDosage = shapeDosageSetter Dosage.inclMinAbsWeightStartDosagePrism
        
        
        let getExclMinAbsWeightStartShapeDosage = shapeDosageGetter Dosage.exclMinAbsWeightStartDosagePrism


        let setExclMinAbsWeightStartShapeDosage = shapeDosageSetter Dosage.exclMinAbsWeightStartDosagePrism

        
        let getInclMaxAbsWeightStartShapeDosage = shapeDosageGetter Dosage.inclMaxAbsWeightStartDosagePrism


        let setInclMaxAbsWeightStartShapeDosage = shapeDosageSetter Dosage.inclMaxAbsWeightStartDosagePrism
        
        
        let getExclMaxAbsWeightStartShapeDosage = shapeDosageGetter Dosage.exclMaxAbsWeightStartDosagePrism


        let setExclMaxAbsWeightStartShapeDosage = shapeDosageSetter Dosage.exclMaxAbsWeightStartDosagePrism


        let getInclMinAbsBSAStartShapeDosage = shapeDosageGetter Dosage.inclMinAbsBSAStartDosagePrism


        let setInclMinAbsBSAStartShapeDosage = shapeDosageSetter Dosage.inclMinAbsBSAStartDosagePrism
        
        
        let getExclMinAbsBSAStartShapeDosage = shapeDosageGetter Dosage.exclMinAbsBSAStartDosagePrism


        let setExclMinAbsBSAStartShapeDosage = shapeDosageSetter Dosage.exclMinAbsBSAStartDosagePrism

        
        let getInclMaxAbsBSAStartShapeDosage = shapeDosageGetter Dosage.inclMaxAbsBSAStartDosagePrism


        let setInclMaxAbsBSAStartShapeDosage = shapeDosageSetter Dosage.inclMaxAbsBSAStartDosagePrism
        
        
        let getExclMaxAbsBSAStartShapeDosage = shapeDosageGetter Dosage.exclMaxAbsBSAStartDosagePrism


        let setExclMaxAbsBSAStartShapeDosage = shapeDosageSetter Dosage.exclMaxAbsBSAStartDosagePrism
    
        
        let getInclMinNormSingleShapeDosage = shapeDosageGetter Dosage.inclMinNormSingleDosagePrism


        let setInclMinNormSingleShapeDosage = shapeDosageSetter Dosage.inclMinNormSingleDosagePrism
        
        
        let getExclMinNormSingleShapeDosage = shapeDosageGetter Dosage.exclMinNormSingleDosagePrism


        let setExclMinNormSingleShapeDosage = shapeDosageSetter Dosage.exclMinNormSingleDosagePrism

        
        let getInclMaxNormSingleShapeDosage = shapeDosageGetter Dosage.inclMaxNormSingleDosagePrism


        let setInclMaxNormSingleShapeDosage = shapeDosageSetter Dosage.inclMaxNormSingleDosagePrism
        
        
        let getExclMaxNormSingleShapeDosage = shapeDosageGetter Dosage.exclMaxNormSingleDosagePrism


        let setExclMaxNormSingleShapeDosage = shapeDosageSetter Dosage.exclMaxNormSingleDosagePrism


        let getInclMinNormWeightSingleShapeDosage = shapeDosageGetter Dosage.inclMinNormWeightSingleDosagePrism


        let setInclMinNormWeightSingleShapeDosage = shapeDosageSetter Dosage.inclMinNormWeightSingleDosagePrism
        
        
        let getExclMinNormWeightSingleShapeDosage = shapeDosageGetter Dosage.exclMinNormWeightSingleDosagePrism


        let setExclMinNormWeightSingleShapeDosage = shapeDosageSetter Dosage.exclMinNormWeightSingleDosagePrism

        
        let getInclMaxNormWeightSingleShapeDosage = shapeDosageGetter Dosage.inclMaxNormWeightSingleDosagePrism


        let setInclMaxNormWeightSingleShapeDosage = shapeDosageSetter Dosage.inclMaxNormWeightSingleDosagePrism
        
        
        let getExclMaxNormWeightSingleShapeDosage = shapeDosageGetter Dosage.exclMaxNormWeightSingleDosagePrism


        let setExclMaxNormWeightSingleShapeDosage = shapeDosageSetter Dosage.exclMaxNormWeightSingleDosagePrism


        let getInclMinNormBSASingleShapeDosage = shapeDosageGetter Dosage.inclMinNormBSASingleDosagePrism


        let setInclMinNormBSASingleShapeDosage = shapeDosageSetter Dosage.inclMinNormBSASingleDosagePrism
        
        
        let getExclMinNormBSASingleShapeDosage = shapeDosageGetter Dosage.exclMinNormBSASingleDosagePrism


        let setExclMinNormBSASingleShapeDosage = shapeDosageSetter Dosage.exclMinNormBSASingleDosagePrism

        
        let getInclMaxNormBSASingleShapeDosage = shapeDosageGetter Dosage.inclMaxNormBSASingleDosagePrism


        let setInclMaxNormBSASingleShapeDosage = shapeDosageSetter Dosage.inclMaxNormBSASingleDosagePrism
        
        
        let getExclMaxNormBSASingleShapeDosage = shapeDosageGetter Dosage.exclMaxNormBSASingleDosagePrism


        let setExclMaxNormBSASingleShapeDosage = shapeDosageSetter Dosage.exclMaxNormBSASingleDosagePrism


        let getInclMinAbsSingleShapeDosage = shapeDosageGetter Dosage.inclMinAbsSingleDosagePrism


        let setInclMinAbsSingleShapeDosage = shapeDosageSetter Dosage.inclMinAbsSingleDosagePrism
        
        
        let getExclMinAbsSingleShapeDosage = shapeDosageGetter Dosage.exclMinAbsSingleDosagePrism


        let setExclMinAbsSingleShapeDosage = shapeDosageSetter Dosage.exclMinAbsSingleDosagePrism

        
        let getInclMaxAbsSingleShapeDosage = shapeDosageGetter Dosage.inclMaxAbsSingleDosagePrism


        let setInclMaxAbsSingleShapeDosage = shapeDosageSetter Dosage.inclMaxAbsSingleDosagePrism
        
        
        let getExclMaxAbsSingleShapeDosage = shapeDosageGetter Dosage.exclMaxAbsSingleDosagePrism


        let setExclMaxAbsSingleShapeDosage = shapeDosageSetter Dosage.exclMaxAbsSingleDosagePrism


        let getInclMinAbsWeightSingleShapeDosage = shapeDosageGetter Dosage.inclMinAbsWeightSingleDosagePrism


        let setInclMinAbsWeightSingleShapeDosage = shapeDosageSetter Dosage.inclMinAbsWeightSingleDosagePrism
        
        
        let getExclMinAbsWeightSingleShapeDosage = shapeDosageGetter Dosage.exclMinAbsWeightSingleDosagePrism


        let setExclMinAbsWeightSingleShapeDosage = shapeDosageSetter Dosage.exclMinAbsWeightSingleDosagePrism

        
        let getInclMaxAbsWeightSingleShapeDosage = shapeDosageGetter Dosage.inclMaxAbsWeightSingleDosagePrism


        let setInclMaxAbsWeightSingleShapeDosage = shapeDosageSetter Dosage.inclMaxAbsWeightSingleDosagePrism
        
        
        let getExclMaxAbsWeightSingleShapeDosage = shapeDosageGetter Dosage.exclMaxAbsWeightSingleDosagePrism


        let setExclMaxAbsWeightSingleShapeDosage = shapeDosageSetter Dosage.exclMaxAbsWeightSingleDosagePrism


        let getInclMinAbsBSASingleShapeDosage = shapeDosageGetter Dosage.inclMinAbsBSASingleDosagePrism


        let setInclMinAbsBSASingleShapeDosage = shapeDosageSetter Dosage.inclMinAbsBSASingleDosagePrism
        
        
        let getExclMinAbsBSASingleShapeDosage = shapeDosageGetter Dosage.exclMinAbsBSASingleDosagePrism


        let setExclMinAbsBSASingleShapeDosage = shapeDosageSetter Dosage.exclMinAbsBSASingleDosagePrism

        
        let getInclMaxAbsBSASingleShapeDosage = shapeDosageGetter Dosage.inclMaxAbsBSASingleDosagePrism


        let setInclMaxAbsBSASingleShapeDosage = shapeDosageSetter Dosage.inclMaxAbsBSASingleDosagePrism
        
        
        let getExclMaxAbsBSASingleShapeDosage = shapeDosageGetter Dosage.exclMaxAbsBSASingleDosagePrism


        let setExclMaxAbsBSASingleShapeDosage = shapeDosageSetter Dosage.exclMaxAbsBSASingleDosagePrism
    
        
        let getInclMinNormRateShapeDosage = shapeDosageGetter Dosage.inclMinNormRateDosagePrism


        let setInclMinNormRateShapeDosage = shapeDosageSetter Dosage.inclMinNormRateDosagePrism
        
        
        let getExclMinNormRateShapeDosage = shapeDosageGetter Dosage.exclMinNormRateDosagePrism


        let setExclMinNormRateShapeDosage = shapeDosageSetter Dosage.exclMinNormRateDosagePrism

        
        let getInclMaxNormRateShapeDosage = shapeDosageGetter Dosage.inclMaxNormRateDosagePrism


        let setInclMaxNormRateShapeDosage = shapeDosageSetter Dosage.inclMaxNormRateDosagePrism
        
        
        let getExclMaxNormRateShapeDosage = shapeDosageGetter Dosage.exclMaxNormRateDosagePrism


        let setExclMaxNormRateShapeDosage = shapeDosageSetter Dosage.exclMaxNormRateDosagePrism


        let getInclMinNormWeightRateShapeDosage = shapeDosageGetter Dosage.inclMinNormWeightRateDosagePrism


        let setInclMinNormWeightRateShapeDosage = shapeDosageSetter Dosage.inclMinNormWeightRateDosagePrism
        
        
        let getExclMinNormWeightRateShapeDosage = shapeDosageGetter Dosage.exclMinNormWeightRateDosagePrism


        let setExclMinNormWeightRateShapeDosage = shapeDosageSetter Dosage.exclMinNormWeightRateDosagePrism

        
        let getInclMaxNormWeightRateShapeDosage = shapeDosageGetter Dosage.inclMaxNormWeightRateDosagePrism


        let setInclMaxNormWeightRateShapeDosage = shapeDosageSetter Dosage.inclMaxNormWeightRateDosagePrism
        
        
        let getExclMaxNormWeightRateShapeDosage = shapeDosageGetter Dosage.exclMaxNormWeightRateDosagePrism


        let setExclMaxNormWeightRateShapeDosage = shapeDosageSetter Dosage.exclMaxNormWeightRateDosagePrism


        let getInclMinNormBSARateShapeDosage = shapeDosageGetter Dosage.inclMinNormBSARateDosagePrism


        let setInclMinNormBSARateShapeDosage = shapeDosageSetter Dosage.inclMinNormBSARateDosagePrism
        
        
        let getExclMinNormBSARateShapeDosage = shapeDosageGetter Dosage.exclMinNormBSARateDosagePrism


        let setExclMinNormBSARateShapeDosage = shapeDosageSetter Dosage.exclMinNormBSARateDosagePrism

        
        let getInclMaxNormBSARateShapeDosage = shapeDosageGetter Dosage.inclMaxNormBSARateDosagePrism


        let setInclMaxNormBSARateShapeDosage = shapeDosageSetter Dosage.inclMaxNormBSARateDosagePrism
        
        
        let getExclMaxNormBSARateShapeDosage = shapeDosageGetter Dosage.exclMaxNormBSARateDosagePrism


        let setExclMaxNormBSARateShapeDosage = shapeDosageSetter Dosage.exclMaxNormBSARateDosagePrism


        let getInclMinAbsRateShapeDosage = shapeDosageGetter Dosage.inclMinAbsRateDosagePrism


        let setInclMinAbsRateShapeDosage = shapeDosageSetter Dosage.inclMinAbsRateDosagePrism
        
        
        let getExclMinAbsRateShapeDosage = shapeDosageGetter Dosage.exclMinAbsRateDosagePrism


        let setExclMinAbsRateShapeDosage = shapeDosageSetter Dosage.exclMinAbsRateDosagePrism

        
        let getInclMaxAbsRateShapeDosage = shapeDosageGetter Dosage.inclMaxAbsRateDosagePrism


        let setInclMaxAbsRateShapeDosage = shapeDosageSetter Dosage.inclMaxAbsRateDosagePrism
        
        
        let getExclMaxAbsRateShapeDosage = shapeDosageGetter Dosage.exclMaxAbsRateDosagePrism


        let setExclMaxAbsRateShapeDosage = shapeDosageSetter Dosage.exclMaxAbsRateDosagePrism


        let getInclMinAbsWeightRateShapeDosage = shapeDosageGetter Dosage.inclMinAbsWeightRateDosagePrism


        let setInclMinAbsWeightRateShapeDosage = shapeDosageSetter Dosage.inclMinAbsWeightRateDosagePrism
        
        
        let getExclMinAbsWeightRateShapeDosage = shapeDosageGetter Dosage.exclMinAbsWeightRateDosagePrism


        let setExclMinAbsWeightRateShapeDosage = shapeDosageSetter Dosage.exclMinAbsWeightRateDosagePrism

        
        let getInclMaxAbsWeightRateShapeDosage = shapeDosageGetter Dosage.inclMaxAbsWeightRateDosagePrism


        let setInclMaxAbsWeightRateShapeDosage = shapeDosageSetter Dosage.inclMaxAbsWeightRateDosagePrism
        
        
        let getExclMaxAbsWeightRateShapeDosage = shapeDosageGetter Dosage.exclMaxAbsWeightRateDosagePrism


        let setExclMaxAbsWeightRateShapeDosage = shapeDosageSetter Dosage.exclMaxAbsWeightRateDosagePrism


        let getInclMinAbsBSARateShapeDosage = shapeDosageGetter Dosage.inclMinAbsBSARateDosagePrism


        let setInclMinAbsBSARateShapeDosage = shapeDosageSetter Dosage.inclMinAbsBSARateDosagePrism
        
        
        let getExclMinAbsBSARateShapeDosage = shapeDosageGetter Dosage.exclMinAbsBSARateDosagePrism


        let setExclMinAbsBSARateShapeDosage = shapeDosageSetter Dosage.exclMinAbsBSARateDosagePrism

        
        let getInclMaxAbsBSARateShapeDosage = shapeDosageGetter Dosage.inclMaxAbsBSARateDosagePrism


        let setInclMaxAbsBSARateShapeDosage = shapeDosageSetter Dosage.inclMaxAbsBSARateDosagePrism
        
        
        let getExclMaxAbsBSARateShapeDosage = shapeDosageGetter Dosage.exclMaxAbsBSARateDosagePrism


        let setExclMaxAbsBSARateShapeDosage = shapeDosageSetter Dosage.exclMaxAbsBSARateDosagePrism

        
        let getInclMinNormTotalShapeDosage = shapeDosageGetter Dosage.inclMinNormTotalDosagePrism


        let setInclMinNormTotalShapeDosage = shapeDosageSetter Dosage.inclMinNormTotalDosagePrism
        
        
        let getExclMinNormTotalShapeDosage = shapeDosageGetter Dosage.exclMinNormTotalDosagePrism


        let setExclMinNormTotalShapeDosage = shapeDosageSetter Dosage.exclMinNormTotalDosagePrism

        
        let getInclMaxNormTotalShapeDosage = shapeDosageGetter Dosage.inclMaxNormTotalDosagePrism


        let setInclMaxNormTotalShapeDosage = shapeDosageSetter Dosage.inclMaxNormTotalDosagePrism
        
        
        let getExclMaxNormTotalShapeDosage = shapeDosageGetter Dosage.exclMaxNormTotalDosagePrism


        let setExclMaxNormTotalShapeDosage = shapeDosageSetter Dosage.exclMaxNormTotalDosagePrism


        let getInclMinNormWeightTotalShapeDosage = shapeDosageGetter Dosage.inclMinNormWeightTotalDosagePrism


        let setInclMinNormWeightTotalShapeDosage = shapeDosageSetter Dosage.inclMinNormWeightTotalDosagePrism
        
        
        let getExclMinNormWeightTotalShapeDosage = shapeDosageGetter Dosage.exclMinNormWeightTotalDosagePrism


        let setExclMinNormWeightTotalShapeDosage = shapeDosageSetter Dosage.exclMinNormWeightTotalDosagePrism

        
        let getInclMaxNormWeightTotalShapeDosage = shapeDosageGetter Dosage.inclMaxNormWeightTotalDosagePrism


        let setInclMaxNormWeightTotalShapeDosage = shapeDosageSetter Dosage.inclMaxNormWeightTotalDosagePrism
        
        
        let getExclMaxNormWeightTotalShapeDosage = shapeDosageGetter Dosage.exclMaxNormWeightTotalDosagePrism


        let setExclMaxNormWeightTotalShapeDosage = shapeDosageSetter Dosage.exclMaxNormWeightTotalDosagePrism


        let getInclMinNormBSATotalShapeDosage = shapeDosageGetter Dosage.inclMinNormBSATotalDosagePrism


        let setInclMinNormBSATotalShapeDosage = shapeDosageSetter Dosage.inclMinNormBSATotalDosagePrism
        
        
        let getExclMinNormBSATotalShapeDosage = shapeDosageGetter Dosage.exclMinNormBSATotalDosagePrism


        let setExclMinNormBSATotalShapeDosage = shapeDosageSetter Dosage.exclMinNormBSATotalDosagePrism

        
        let getInclMaxNormBSATotalShapeDosage = shapeDosageGetter Dosage.inclMaxNormBSATotalDosagePrism


        let setInclMaxNormBSATotalShapeDosage = shapeDosageSetter Dosage.inclMaxNormBSATotalDosagePrism
        
        
        let getExclMaxNormBSATotalShapeDosage = shapeDosageGetter Dosage.exclMaxNormBSATotalDosagePrism


        let setExclMaxNormBSATotalShapeDosage = shapeDosageSetter Dosage.exclMaxNormBSATotalDosagePrism


        let getInclMinAbsTotalShapeDosage = shapeDosageGetter Dosage.inclMinAbsTotalDosagePrism


        let setInclMinAbsTotalShapeDosage = shapeDosageSetter Dosage.inclMinAbsTotalDosagePrism
        
        
        let getExclMinAbsTotalShapeDosage = shapeDosageGetter Dosage.exclMinAbsTotalDosagePrism


        let setExclMinAbsTotalShapeDosage = shapeDosageSetter Dosage.exclMinAbsTotalDosagePrism

        
        let getInclMaxAbsTotalShapeDosage = shapeDosageGetter Dosage.inclMaxAbsTotalDosagePrism


        let setInclMaxAbsTotalShapeDosage = shapeDosageSetter Dosage.inclMaxAbsTotalDosagePrism
        
        
        let getExclMaxAbsTotalShapeDosage = shapeDosageGetter Dosage.exclMaxAbsTotalDosagePrism


        let setExclMaxAbsTotalShapeDosage = shapeDosageSetter Dosage.exclMaxAbsTotalDosagePrism


        let getInclMinAbsWeightTotalShapeDosage = shapeDosageGetter Dosage.inclMinAbsWeightTotalDosagePrism


        let setInclMinAbsWeightTotalShapeDosage = shapeDosageSetter Dosage.inclMinAbsWeightTotalDosagePrism
        
        
        let getExclMinAbsWeightTotalShapeDosage = shapeDosageGetter Dosage.exclMinAbsWeightTotalDosagePrism


        let setExclMinAbsWeightTotalShapeDosage = shapeDosageSetter Dosage.exclMinAbsWeightTotalDosagePrism

        
        let getInclMaxAbsWeightTotalShapeDosage = shapeDosageGetter Dosage.inclMaxAbsWeightTotalDosagePrism


        let setInclMaxAbsWeightTotalShapeDosage = shapeDosageSetter Dosage.inclMaxAbsWeightTotalDosagePrism
        
        
        let getExclMaxAbsWeightTotalShapeDosage = shapeDosageGetter Dosage.exclMaxAbsWeightTotalDosagePrism


        let setExclMaxAbsWeightTotalShapeDosage = shapeDosageSetter Dosage.exclMaxAbsWeightTotalDosagePrism


        let getInclMinAbsBSATotalShapeDosage = shapeDosageGetter Dosage.inclMinAbsBSATotalDosagePrism


        let setInclMinAbsBSATotalShapeDosage = shapeDosageSetter Dosage.inclMinAbsBSATotalDosagePrism
        
        
        let getExclMinAbsBSATotalShapeDosage = shapeDosageGetter Dosage.exclMinAbsBSATotalDosagePrism


        let setExclMinAbsBSATotalShapeDosage = shapeDosageSetter Dosage.exclMinAbsBSATotalDosagePrism

        
        let getInclMaxAbsBSATotalShapeDosage = shapeDosageGetter Dosage.inclMaxAbsBSATotalDosagePrism


        let setInclMaxAbsBSATotalShapeDosage = shapeDosageSetter Dosage.inclMaxAbsBSATotalDosagePrism
        
        
        let getExclMaxAbsBSATotalShapeDosage = shapeDosageGetter Dosage.exclMaxAbsBSATotalDosagePrism


        let setExclMaxAbsBSATotalShapeDosage = shapeDosageSetter Dosage.exclMaxAbsBSATotalDosagePrism
    

        let substanceDosagesPrism n1 n2 n3 n4 =
            patientDosagePrism n1 n2 n3 n4 >?> PatientDosage.SubstanceDosages_ 
             
        
        let getSubstanceDosages inds rt shp pat dr =
            match dr |> indxPatient inds rt shp pat with 
            | Some (ni, nr, np, ns) -> 
                match dr |> Optic.get (substanceDosagesPrism ni nr np ns) with
                | Some sds -> sds
                | None -> []
            | None -> []    
        
        
        let setSubstanceDosages inds rt shp pat sds dr =
            match dr |> indxPatient inds rt shp pat with 
            | Some (ni, nr, np, ns) -> 
                dr 
                |> Optic.set (substanceDosagesPrism ni nr np ns) sds 
            | None -> dr

        
        let addSubstance inds rt shp pat sn dr  =
            match sn |> createSubstanceDosage with
            | None -> dr
            | Some sds ->
                match dr |> indxSubstance inds rt shp pat sn with
                | Some _ -> dr
                | None ->
                    let sds =
                        dr
                        |> getSubstanceDosages inds rt shp pat
                        |> List.prepend [sds]
        
                    dr
                    |> setSubstanceDosages inds rt shp pat sds

    
        let substanceDosagePrism n1 n2 n3 n4 n5 =
            substanceDosagesPrism n1 n2 n3 n4  >?> List.pos_ n5

    
        let inline private substanceDosageGetter prism inds rt shp pat sn dr = 
            match dr |> indxSubstance inds rt shp pat sn with 
            | Some (ni, nr, np, ns, n) ->
                dr |> Optic.get ((substanceDosagePrism ni nr np ns n) >?> prism)
            | None -> None


        let inline private substanceDosageSetter prism inds rt shp pat sn vu dr = 
            match dr |> indxSubstance inds rt shp pat sn with 
            | Some (ni, nr, np, ns, n) ->
                dr |> Optic.set ((substanceDosagePrism ni nr np ns n) >?> prism) vu
            | None -> dr


        let getRateUnitSubstanceDosage = substanceDosageGetter Dosage.rateUnitRateDosagePrism


        let setRateUnitSubstanceDosage = substanceDosageSetter Dosage.rateUnitRateDosagePrism


        let getTimeUnitSubstanceDosage = substanceDosageGetter Dosage.timeUnitTotalDosagePrism


        let setTimeUnitSubstanceDosage = substanceDosageSetter Dosage.timeUnitTotalDosagePrism


        let getNormWeightUnitStartSubstanceDosage = substanceDosageGetter Dosage.normWeightUnitStartDosagePrism


        let setNormWeightUnitStartSubstanceDosage = substanceDosageSetter Dosage.normWeightUnitStartDosagePrism


        let getNormBSAUnitStartSubstanceDosage = substanceDosageGetter Dosage.normBSAUnitStartDosagePrism


        let setNormBSAUnitStartSubstanceDosage = substanceDosageSetter Dosage.normBSAUnitStartDosagePrism


        let getNormWeightUnitSingleSubstanceDosage = substanceDosageGetter Dosage.normWeightUnitSingleDosagePrism


        let setNormWeightUnitSingleSubstanceDosage = substanceDosageSetter Dosage.normWeightUnitSingleDosagePrism


        let getNormBSAUnitSingleSubstanceDosage = substanceDosageGetter Dosage.normBSAUnitSingleDosagePrism


        let setNormBSAUnitSingleSubstanceDosage = substanceDosageSetter Dosage.normBSAUnitSingleDosagePrism

    
        let getNormWeightUnitRateSubstanceDosage = substanceDosageGetter Dosage.normWeightUnitRateDosagePrism


        let setNormWeightUnitRateSubstanceDosage = substanceDosageSetter Dosage.normWeightUnitRateDosagePrism


        let getNormBSAUnitRateSubstanceDosage = substanceDosageGetter Dosage.normBSAUnitRateDosagePrism


        let setNormBSAUnitRateSubstanceDosage = substanceDosageSetter Dosage.normBSAUnitRateDosagePrism


        let getNormWeightUnitTotalSubstanceDosage = substanceDosageGetter Dosage.normWeightUnitTotalDosagePrism


        let setNormWeightUnitTotalSubstanceDosage = substanceDosageSetter Dosage.normWeightUnitTotalDosagePrism


        let getNormBSAUnitTotalSubstanceDosage = substanceDosageGetter Dosage.normBSAUnitTotalDosagePrism


        let setNormBSAUnitTotalSubstanceDosage = substanceDosageSetter Dosage.normBSAUnitTotalDosagePrism


        let getAbsWeightUnitStartSubstanceDosage = substanceDosageGetter Dosage.absWeightUnitStartDosagePrism


        let setAbsWeightUnitStartSubstanceDosage = substanceDosageSetter Dosage.absWeightUnitStartDosagePrism


        let getAbsBSAUnitStartSubstanceDosage = substanceDosageGetter Dosage.absBSAUnitStartDosagePrism


        let setAbsBSAUnitStartSubstanceDosage = substanceDosageSetter Dosage.absBSAUnitStartDosagePrism


        let getAbsWeightUnitSingleSubstanceDosage = substanceDosageGetter Dosage.absWeightUnitSingleDosagePrism


        let setAbsWeightUnitSingleSubstanceDosage = substanceDosageSetter Dosage.absWeightUnitSingleDosagePrism


        let getAbsBSAUnitSingleSubstanceDosage = substanceDosageGetter Dosage.absBSAUnitSingleDosagePrism


        let setAbsBSAUnitSingleSubstanceDosage = substanceDosageSetter Dosage.absBSAUnitSingleDosagePrism

    
        let getAbsWeightUnitRateSubstanceDosage = substanceDosageGetter Dosage.absWeightUnitRateDosagePrism


        let setAbsWeightUnitRateSubstanceDosage = substanceDosageSetter Dosage.absWeightUnitRateDosagePrism


        let getAbsBSAUnitRateSubstanceDosage = substanceDosageGetter Dosage.absBSAUnitRateDosagePrism


        let setAbsBSAUnitRateSubstanceDosage = substanceDosageSetter Dosage.absBSAUnitRateDosagePrism


        let getAbsWeightUnitTotalSubstanceDosage = substanceDosageGetter Dosage.absWeightUnitTotalDosagePrism


        let setAbsWeightUnitTotalSubstanceDosage = substanceDosageSetter Dosage.absWeightUnitTotalDosagePrism


        let getAbsBSAUnitTotalSubstanceDosage = substanceDosageGetter Dosage.absBSAUnitTotalDosagePrism


        let setAbsBSAUnitTotalSubstanceDosage = substanceDosageSetter Dosage.absBSAUnitTotalDosagePrism


        let getFreqsFrequencySubstanceDosage = substanceDosageGetter Dosage.freqsFrequencyLens


        let setFreqsFrequencySubstanceDosage = substanceDosageSetter Dosage.freqsFrequencyLens


        let getTimeUnitFrequencySubstanceDosage = substanceDosageGetter Dosage.timeUnitFrequencyLens


        let setTimeUnitFrequencySubstanceDosage = substanceDosageSetter Dosage.timeUnitFrequencyLens
        

        let getMinIntervalFrequencySubstanceDosage = substanceDosageGetter Dosage.minIntervalValueFrequencyLens
        

        let setMinIntervalFrequencySubstanceDosage = substanceDosageSetter Dosage.minIntervalValueFrequencyLens

        
        let getInclMinNormStartSubstanceDosage = substanceDosageGetter Dosage.inclMinNormStartDosagePrism


        let setInclMinNormStartSubstanceDosage = substanceDosageSetter Dosage.inclMinNormStartDosagePrism
        
        
        let getExclMinNormStartSubstanceDosage = substanceDosageGetter Dosage.exclMinNormStartDosagePrism


        let setExclMinNormStartSubstanceDosage = substanceDosageSetter Dosage.exclMinNormStartDosagePrism

        
        let getInclMaxNormStartSubstanceDosage = substanceDosageGetter Dosage.inclMaxNormStartDosagePrism


        let setInclMaxNormStartSubstanceDosage = substanceDosageSetter Dosage.inclMaxNormStartDosagePrism
        
        
        let getExclMaxNormStartSubstanceDosage = substanceDosageGetter Dosage.exclMaxNormStartDosagePrism


        let setExclMaxNormStartSubstanceDosage = substanceDosageSetter Dosage.exclMaxNormStartDosagePrism


        let getInclMinNormWeightStartSubstanceDosage = substanceDosageGetter Dosage.inclMinNormWeightStartDosagePrism


        let setInclMinNormWeightStartSubstanceDosage = substanceDosageSetter Dosage.inclMinNormWeightStartDosagePrism
        
        
        let getExclMinNormWeightStartSubstanceDosage = substanceDosageGetter Dosage.exclMinNormWeightStartDosagePrism


        let setExclMinNormWeightStartSubstanceDosage = substanceDosageSetter Dosage.exclMinNormWeightStartDosagePrism

        
        let getInclMaxNormWeightStartSubstanceDosage = substanceDosageGetter Dosage.inclMaxNormWeightStartDosagePrism


        let setInclMaxNormWeightStartSubstanceDosage = substanceDosageSetter Dosage.inclMaxNormWeightStartDosagePrism
        
        
        let getExclMaxNormWeightStartSubstanceDosage = substanceDosageGetter Dosage.exclMaxNormWeightStartDosagePrism


        let setExclMaxNormWeightStartSubstanceDosage = substanceDosageSetter Dosage.exclMaxNormWeightStartDosagePrism


        let getInclMinNormBSAStartSubstanceDosage = substanceDosageGetter Dosage.inclMinNormBSAStartDosagePrism


        let setInclMinNormBSAStartSubstanceDosage = substanceDosageSetter Dosage.inclMinNormBSAStartDosagePrism
        
        
        let getExclMinNormBSAStartSubstanceDosage = substanceDosageGetter Dosage.exclMinNormBSAStartDosagePrism


        let setExclMinNormBSAStartSubstanceDosage = substanceDosageSetter Dosage.exclMinNormBSAStartDosagePrism

        
        let getInclMaxNormBSAStartSubstanceDosage = substanceDosageGetter Dosage.inclMaxNormBSAStartDosagePrism


        let setInclMaxNormBSAStartSubstanceDosage = substanceDosageSetter Dosage.inclMaxNormBSAStartDosagePrism
        
        
        let getExclMaxNormBSAStartSubstanceDosage = substanceDosageGetter Dosage.exclMaxNormBSAStartDosagePrism


        let setExclMaxNormBSAStartSubstanceDosage = substanceDosageSetter Dosage.exclMaxNormBSAStartDosagePrism


        let getInclMinAbsStartSubstanceDosage = substanceDosageGetter Dosage.inclMinAbsStartDosagePrism


        let setInclMinAbsStartSubstanceDosage = substanceDosageSetter Dosage.inclMinAbsStartDosagePrism
        
        
        let getExclMinAbsStartSubstanceDosage = substanceDosageGetter Dosage.exclMinAbsStartDosagePrism


        let setExclMinAbsStartSubstanceDosage = substanceDosageSetter Dosage.exclMinAbsStartDosagePrism

        
        let getInclMaxAbsStartSubstanceDosage = substanceDosageGetter Dosage.inclMaxAbsStartDosagePrism


        let setInclMaxAbsStartSubstanceDosage = substanceDosageSetter Dosage.inclMaxAbsStartDosagePrism
        
        
        let getExclMaxAbsStartSubstanceDosage = substanceDosageGetter Dosage.exclMaxAbsStartDosagePrism


        let setExclMaxAbsStartSubstanceDosage = substanceDosageSetter Dosage.exclMaxAbsStartDosagePrism


        let getInclMinAbsWeightStartSubstanceDosage = substanceDosageGetter Dosage.inclMinAbsWeightStartDosagePrism


        let setInclMinAbsWeightStartSubstanceDosage = substanceDosageSetter Dosage.inclMinAbsWeightStartDosagePrism
        
        
        let getExclMinAbsWeightStartSubstanceDosage = substanceDosageGetter Dosage.exclMinAbsWeightStartDosagePrism


        let setExclMinAbsWeightStartSubstanceDosage = substanceDosageSetter Dosage.exclMinAbsWeightStartDosagePrism

        
        let getInclMaxAbsWeightStartSubstanceDosage = substanceDosageGetter Dosage.inclMaxAbsWeightStartDosagePrism


        let setInclMaxAbsWeightStartSubstanceDosage = substanceDosageSetter Dosage.inclMaxAbsWeightStartDosagePrism
        
        
        let getExclMaxAbsWeightStartSubstanceDosage = substanceDosageGetter Dosage.exclMaxAbsWeightStartDosagePrism


        let setExclMaxAbsWeightStartSubstanceDosage = substanceDosageSetter Dosage.exclMaxAbsWeightStartDosagePrism


        let getInclMinAbsBSAStartSubstanceDosage = substanceDosageGetter Dosage.inclMinAbsBSAStartDosagePrism


        let setInclMinAbsBSAStartSubstanceDosage = substanceDosageSetter Dosage.inclMinAbsBSAStartDosagePrism
        
        
        let getExclMinAbsBSAStartSubstanceDosage = substanceDosageGetter Dosage.exclMinAbsBSAStartDosagePrism


        let setExclMinAbsBSAStartSubstanceDosage = substanceDosageSetter Dosage.exclMinAbsBSAStartDosagePrism

        
        let getInclMaxAbsBSAStartSubstanceDosage = substanceDosageGetter Dosage.inclMaxAbsBSAStartDosagePrism


        let setInclMaxAbsBSAStartSubstanceDosage = substanceDosageSetter Dosage.inclMaxAbsBSAStartDosagePrism
        
        
        let getExclMaxAbsBSAStartSubstanceDosage = substanceDosageGetter Dosage.exclMaxAbsBSAStartDosagePrism


        let setExclMaxAbsBSAStartSubstanceDosage = substanceDosageSetter Dosage.exclMaxAbsBSAStartDosagePrism
    
        
        let getInclMinNormSingleSubstanceDosage = substanceDosageGetter Dosage.inclMinNormSingleDosagePrism


        let setInclMinNormSingleSubstanceDosage = substanceDosageSetter Dosage.inclMinNormSingleDosagePrism
        
        
        let getExclMinNormSingleSubstanceDosage = substanceDosageGetter Dosage.exclMinNormSingleDosagePrism


        let setExclMinNormSingleSubstanceDosage = substanceDosageSetter Dosage.exclMinNormSingleDosagePrism

        
        let getInclMaxNormSingleSubstanceDosage = substanceDosageGetter Dosage.inclMaxNormSingleDosagePrism


        let setInclMaxNormSingleSubstanceDosage = substanceDosageSetter Dosage.inclMaxNormSingleDosagePrism
        
        
        let getExclMaxNormSingleSubstanceDosage = substanceDosageGetter Dosage.exclMaxNormSingleDosagePrism


        let setExclMaxNormSingleSubstanceDosage = substanceDosageSetter Dosage.exclMaxNormSingleDosagePrism


        let getInclMinNormWeightSingleSubstanceDosage = substanceDosageGetter Dosage.inclMinNormWeightSingleDosagePrism


        let setInclMinNormWeightSingleSubstanceDosage = substanceDosageSetter Dosage.inclMinNormWeightSingleDosagePrism
        
        
        let getExclMinNormWeightSingleSubstanceDosage = substanceDosageGetter Dosage.exclMinNormWeightSingleDosagePrism


        let setExclMinNormWeightSingleSubstanceDosage = substanceDosageSetter Dosage.exclMinNormWeightSingleDosagePrism

        
        let getInclMaxNormWeightSingleSubstanceDosage = substanceDosageGetter Dosage.inclMaxNormWeightSingleDosagePrism


        let setInclMaxNormWeightSingleSubstanceDosage = substanceDosageSetter Dosage.inclMaxNormWeightSingleDosagePrism
        
        
        let getExclMaxNormWeightSingleSubstanceDosage = substanceDosageGetter Dosage.exclMaxNormWeightSingleDosagePrism


        let setExclMaxNormWeightSingleSubstanceDosage = substanceDosageSetter Dosage.exclMaxNormWeightSingleDosagePrism


        let getInclMinNormBSASingleSubstanceDosage = substanceDosageGetter Dosage.inclMinNormBSASingleDosagePrism


        let setInclMinNormBSASingleSubstanceDosage = substanceDosageSetter Dosage.inclMinNormBSASingleDosagePrism
        
        
        let getExclMinNormBSASingleSubstanceDosage = substanceDosageGetter Dosage.exclMinNormBSASingleDosagePrism


        let setExclMinNormBSASingleSubstanceDosage = substanceDosageSetter Dosage.exclMinNormBSASingleDosagePrism

        
        let getInclMaxNormBSASingleSubstanceDosage = substanceDosageGetter Dosage.inclMaxNormBSASingleDosagePrism


        let setInclMaxNormBSASingleSubstanceDosage = substanceDosageSetter Dosage.inclMaxNormBSASingleDosagePrism
        
        
        let getExclMaxNormBSASingleSubstanceDosage = substanceDosageGetter Dosage.exclMaxNormBSASingleDosagePrism


        let setExclMaxNormBSASingleSubstanceDosage = substanceDosageSetter Dosage.exclMaxNormBSASingleDosagePrism


        let getInclMinAbsSingleSubstanceDosage = substanceDosageGetter Dosage.inclMinAbsSingleDosagePrism


        let setInclMinAbsSingleSubstanceDosage = substanceDosageSetter Dosage.inclMinAbsSingleDosagePrism
        
        
        let getExclMinAbsSingleSubstanceDosage = substanceDosageGetter Dosage.exclMinAbsSingleDosagePrism


        let setExclMinAbsSingleSubstanceDosage = substanceDosageSetter Dosage.exclMinAbsSingleDosagePrism

        
        let getInclMaxAbsSingleSubstanceDosage = substanceDosageGetter Dosage.inclMaxAbsSingleDosagePrism


        let setInclMaxAbsSingleSubstanceDosage = substanceDosageSetter Dosage.inclMaxAbsSingleDosagePrism
        
        
        let getExclMaxAbsSingleSubstanceDosage = substanceDosageGetter Dosage.exclMaxAbsSingleDosagePrism


        let setExclMaxAbsSingleSubstanceDosage = substanceDosageSetter Dosage.exclMaxAbsSingleDosagePrism


        let getInclMinAbsWeightSingleSubstanceDosage = substanceDosageGetter Dosage.inclMinAbsWeightSingleDosagePrism


        let setInclMinAbsWeightSingleSubstanceDosage = substanceDosageSetter Dosage.inclMinAbsWeightSingleDosagePrism
        
        
        let getExclMinAbsWeightSingleSubstanceDosage = substanceDosageGetter Dosage.exclMinAbsWeightSingleDosagePrism


        let setExclMinAbsWeightSingleSubstanceDosage = substanceDosageSetter Dosage.exclMinAbsWeightSingleDosagePrism

        
        let getInclMaxAbsWeightSingleSubstanceDosage = substanceDosageGetter Dosage.inclMaxAbsWeightSingleDosagePrism


        let setInclMaxAbsWeightSingleSubstanceDosage = substanceDosageSetter Dosage.inclMaxAbsWeightSingleDosagePrism
        
        
        let getExclMaxAbsWeightSingleSubstanceDosage = substanceDosageGetter Dosage.exclMaxAbsWeightSingleDosagePrism


        let setExclMaxAbsWeightSingleSubstanceDosage = substanceDosageSetter Dosage.exclMaxAbsWeightSingleDosagePrism


        let getInclMinAbsBSASingleSubstanceDosage = substanceDosageGetter Dosage.inclMinAbsBSASingleDosagePrism


        let setInclMinAbsBSASingleSubstanceDosage = substanceDosageSetter Dosage.inclMinAbsBSASingleDosagePrism
        
        
        let getExclMinAbsBSASingleSubstanceDosage = substanceDosageGetter Dosage.exclMinAbsBSASingleDosagePrism


        let setExclMinAbsBSASingleSubstanceDosage = substanceDosageSetter Dosage.exclMinAbsBSASingleDosagePrism

        
        let getInclMaxAbsBSASingleSubstanceDosage = substanceDosageGetter Dosage.inclMaxAbsBSASingleDosagePrism


        let setInclMaxAbsBSASingleSubstanceDosage = substanceDosageSetter Dosage.inclMaxAbsBSASingleDosagePrism
        
        
        let getExclMaxAbsBSASingleSubstanceDosage = substanceDosageGetter Dosage.exclMaxAbsBSASingleDosagePrism


        let setExclMaxAbsBSASingleSubstanceDosage = substanceDosageSetter Dosage.exclMaxAbsBSASingleDosagePrism
    
        
        let getInclMinNormRateSubstanceDosage = substanceDosageGetter Dosage.inclMinNormRateDosagePrism


        let setInclMinNormRateSubstanceDosage = substanceDosageSetter Dosage.inclMinNormRateDosagePrism
        
        
        let getExclMinNormRateSubstanceDosage = substanceDosageGetter Dosage.exclMinNormRateDosagePrism


        let setExclMinNormRateSubstanceDosage = substanceDosageSetter Dosage.exclMinNormRateDosagePrism

        
        let getInclMaxNormRateSubstanceDosage = substanceDosageGetter Dosage.inclMaxNormRateDosagePrism


        let setInclMaxNormRateSubstanceDosage = substanceDosageSetter Dosage.inclMaxNormRateDosagePrism
        
        
        let getExclMaxNormRateSubstanceDosage = substanceDosageGetter Dosage.exclMaxNormRateDosagePrism


        let setExclMaxNormRateSubstanceDosage = substanceDosageSetter Dosage.exclMaxNormRateDosagePrism


        let getInclMinNormWeightRateSubstanceDosage = substanceDosageGetter Dosage.inclMinNormWeightRateDosagePrism


        let setInclMinNormWeightRateSubstanceDosage = substanceDosageSetter Dosage.inclMinNormWeightRateDosagePrism
        
        
        let getExclMinNormWeightRateSubstanceDosage = substanceDosageGetter Dosage.exclMinNormWeightRateDosagePrism


        let setExclMinNormWeightRateSubstanceDosage = substanceDosageSetter Dosage.exclMinNormWeightRateDosagePrism

        
        let getInclMaxNormWeightRateSubstanceDosage = substanceDosageGetter Dosage.inclMaxNormWeightRateDosagePrism


        let setInclMaxNormWeightRateSubstanceDosage = substanceDosageSetter Dosage.inclMaxNormWeightRateDosagePrism
        
        
        let getExclMaxNormWeightRateSubstanceDosage = substanceDosageGetter Dosage.exclMaxNormWeightRateDosagePrism


        let setExclMaxNormWeightRateSubstanceDosage = substanceDosageSetter Dosage.exclMaxNormWeightRateDosagePrism


        let getInclMinNormBSARateSubstanceDosage = substanceDosageGetter Dosage.inclMinNormBSARateDosagePrism


        let setInclMinNormBSARateSubstanceDosage = substanceDosageSetter Dosage.inclMinNormBSARateDosagePrism
        
        
        let getExclMinNormBSARateSubstanceDosage = substanceDosageGetter Dosage.exclMinNormBSARateDosagePrism


        let setExclMinNormBSARateSubstanceDosage = substanceDosageSetter Dosage.exclMinNormBSARateDosagePrism

        
        let getInclMaxNormBSARateSubstanceDosage = substanceDosageGetter Dosage.inclMaxNormBSARateDosagePrism


        let setInclMaxNormBSARateSubstanceDosage = substanceDosageSetter Dosage.inclMaxNormBSARateDosagePrism
        
        
        let getExclMaxNormBSARateSubstanceDosage = substanceDosageGetter Dosage.exclMaxNormBSARateDosagePrism


        let setExclMaxNormBSARateSubstanceDosage = substanceDosageSetter Dosage.exclMaxNormBSARateDosagePrism


        let getInclMinAbsRateSubstanceDosage = substanceDosageGetter Dosage.inclMinAbsRateDosagePrism


        let setInclMinAbsRateSubstanceDosage = substanceDosageSetter Dosage.inclMinAbsRateDosagePrism
        
        
        let getExclMinAbsRateSubstanceDosage = substanceDosageGetter Dosage.exclMinAbsRateDosagePrism


        let setExclMinAbsRateSubstanceDosage = substanceDosageSetter Dosage.exclMinAbsRateDosagePrism

        
        let getInclMaxAbsRateSubstanceDosage = substanceDosageGetter Dosage.inclMaxAbsRateDosagePrism


        let setInclMaxAbsRateSubstanceDosage = substanceDosageSetter Dosage.inclMaxAbsRateDosagePrism
        
        
        let getExclMaxAbsRateSubstanceDosage = substanceDosageGetter Dosage.exclMaxAbsRateDosagePrism


        let setExclMaxAbsRateSubstanceDosage = substanceDosageSetter Dosage.exclMaxAbsRateDosagePrism


        let getInclMinAbsWeightRateSubstanceDosage = substanceDosageGetter Dosage.inclMinAbsWeightRateDosagePrism


        let setInclMinAbsWeightRateSubstanceDosage = substanceDosageSetter Dosage.inclMinAbsWeightRateDosagePrism
        
        
        let getExclMinAbsWeightRateSubstanceDosage = substanceDosageGetter Dosage.exclMinAbsWeightRateDosagePrism


        let setExclMinAbsWeightRateSubstanceDosage = substanceDosageSetter Dosage.exclMinAbsWeightRateDosagePrism

        
        let getInclMaxAbsWeightRateSubstanceDosage = substanceDosageGetter Dosage.inclMaxAbsWeightRateDosagePrism


        let setInclMaxAbsWeightRateSubstanceDosage = substanceDosageSetter Dosage.inclMaxAbsWeightRateDosagePrism
        
        
        let getExclMaxAbsWeightRateSubstanceDosage = substanceDosageGetter Dosage.exclMaxAbsWeightRateDosagePrism


        let setExclMaxAbsWeightRateSubstanceDosage = substanceDosageSetter Dosage.exclMaxAbsWeightRateDosagePrism


        let getInclMinAbsBSARateSubstanceDosage = substanceDosageGetter Dosage.inclMinAbsBSARateDosagePrism


        let setInclMinAbsBSARateSubstanceDosage = substanceDosageSetter Dosage.inclMinAbsBSARateDosagePrism
        
        
        let getExclMinAbsBSARateSubstanceDosage = substanceDosageGetter Dosage.exclMinAbsBSARateDosagePrism


        let setExclMinAbsBSARateSubstanceDosage = substanceDosageSetter Dosage.exclMinAbsBSARateDosagePrism

        
        let getInclMaxAbsBSARateSubstanceDosage = substanceDosageGetter Dosage.inclMaxAbsBSARateDosagePrism


        let setInclMaxAbsBSARateSubstanceDosage = substanceDosageSetter Dosage.inclMaxAbsBSARateDosagePrism
        
        
        let getExclMaxAbsBSARateSubstanceDosage = substanceDosageGetter Dosage.exclMaxAbsBSARateDosagePrism


        let setExclMaxAbsBSARateSubstanceDosage = substanceDosageSetter Dosage.exclMaxAbsBSARateDosagePrism

        
        let getInclMinNormTotalSubstanceDosage = substanceDosageGetter Dosage.inclMinNormTotalDosagePrism


        let setInclMinNormTotalSubstanceDosage = substanceDosageSetter Dosage.inclMinNormTotalDosagePrism
        
        
        let getExclMinNormTotalSubstanceDosage = substanceDosageGetter Dosage.exclMinNormTotalDosagePrism


        let setExclMinNormTotalSubstanceDosage = substanceDosageSetter Dosage.exclMinNormTotalDosagePrism

        
        let getInclMaxNormTotalSubstanceDosage = substanceDosageGetter Dosage.inclMaxNormTotalDosagePrism


        let setInclMaxNormTotalSubstanceDosage = substanceDosageSetter Dosage.inclMaxNormTotalDosagePrism
        
        
        let getExclMaxNormTotalSubstanceDosage = substanceDosageGetter Dosage.exclMaxNormTotalDosagePrism


        let setExclMaxNormTotalSubstanceDosage = substanceDosageSetter Dosage.exclMaxNormTotalDosagePrism


        let getInclMinNormWeightTotalSubstanceDosage = substanceDosageGetter Dosage.inclMinNormWeightTotalDosagePrism


        let setInclMinNormWeightTotalSubstanceDosage = substanceDosageSetter Dosage.inclMinNormWeightTotalDosagePrism
        
        
        let getExclMinNormWeightTotalSubstanceDosage = substanceDosageGetter Dosage.exclMinNormWeightTotalDosagePrism


        let setExclMinNormWeightTotalSubstanceDosage = substanceDosageSetter Dosage.exclMinNormWeightTotalDosagePrism

        
        let getInclMaxNormWeightTotalSubstanceDosage = substanceDosageGetter Dosage.inclMaxNormWeightTotalDosagePrism


        let setInclMaxNormWeightTotalSubstanceDosage = substanceDosageSetter Dosage.inclMaxNormWeightTotalDosagePrism
        
        
        let getExclMaxNormWeightTotalSubstanceDosage = substanceDosageGetter Dosage.exclMaxNormWeightTotalDosagePrism


        let setExclMaxNormWeightTotalSubstanceDosage = substanceDosageSetter Dosage.exclMaxNormWeightTotalDosagePrism


        let getInclMinNormBSATotalSubstanceDosage = substanceDosageGetter Dosage.inclMinNormBSATotalDosagePrism


        let setInclMinNormBSATotalSubstanceDosage = substanceDosageSetter Dosage.inclMinNormBSATotalDosagePrism
        
        
        let getExclMinNormBSATotalSubstanceDosage = substanceDosageGetter Dosage.exclMinNormBSATotalDosagePrism


        let setExclMinNormBSATotalSubstanceDosage = substanceDosageSetter Dosage.exclMinNormBSATotalDosagePrism

        
        let getInclMaxNormBSATotalSubstanceDosage = substanceDosageGetter Dosage.inclMaxNormBSATotalDosagePrism


        let setInclMaxNormBSATotalSubstanceDosage = substanceDosageSetter Dosage.inclMaxNormBSATotalDosagePrism
        
        
        let getExclMaxNormBSATotalSubstanceDosage = substanceDosageGetter Dosage.exclMaxNormBSATotalDosagePrism


        let setExclMaxNormBSATotalSubstanceDosage = substanceDosageSetter Dosage.exclMaxNormBSATotalDosagePrism


        let getInclMinAbsTotalSubstanceDosage = substanceDosageGetter Dosage.inclMinAbsTotalDosagePrism


        let setInclMinAbsTotalSubstanceDosage = substanceDosageSetter Dosage.inclMinAbsTotalDosagePrism
        
        
        let getExclMinAbsTotalSubstanceDosage = substanceDosageGetter Dosage.exclMinAbsTotalDosagePrism


        let setExclMinAbsTotalSubstanceDosage = substanceDosageSetter Dosage.exclMinAbsTotalDosagePrism

        
        let getInclMaxAbsTotalSubstanceDosage = substanceDosageGetter Dosage.inclMaxAbsTotalDosagePrism


        let setInclMaxAbsTotalSubstanceDosage = substanceDosageSetter Dosage.inclMaxAbsTotalDosagePrism
        
        
        let getExclMaxAbsTotalSubstanceDosage = substanceDosageGetter Dosage.exclMaxAbsTotalDosagePrism


        let setExclMaxAbsTotalSubstanceDosage = substanceDosageSetter Dosage.exclMaxAbsTotalDosagePrism


        let getInclMinAbsWeightTotalSubstanceDosage = substanceDosageGetter Dosage.inclMinAbsWeightTotalDosagePrism


        let setInclMinAbsWeightTotalSubstanceDosage = substanceDosageSetter Dosage.inclMinAbsWeightTotalDosagePrism
        
        
        let getExclMinAbsWeightTotalSubstanceDosage = substanceDosageGetter Dosage.exclMinAbsWeightTotalDosagePrism


        let setExclMinAbsWeightTotalSubstanceDosage = substanceDosageSetter Dosage.exclMinAbsWeightTotalDosagePrism

        
        let getInclMaxAbsWeightTotalSubstanceDosage = substanceDosageGetter Dosage.inclMaxAbsWeightTotalDosagePrism


        let setInclMaxAbsWeightTotalSubstanceDosage = substanceDosageSetter Dosage.inclMaxAbsWeightTotalDosagePrism
        
        
        let getExclMaxAbsWeightTotalSubstanceDosage = substanceDosageGetter Dosage.exclMaxAbsWeightTotalDosagePrism


        let setExclMaxAbsWeightTotalSubstanceDosage = substanceDosageSetter Dosage.exclMaxAbsWeightTotalDosagePrism


        let getInclMinAbsBSATotalSubstanceDosage = substanceDosageGetter Dosage.inclMinAbsBSATotalDosagePrism


        let setInclMinAbsBSATotalSubstanceDosage = substanceDosageSetter Dosage.inclMinAbsBSATotalDosagePrism
        
        
        let getExclMinAbsBSATotalSubstanceDosage = substanceDosageGetter Dosage.exclMinAbsBSATotalDosagePrism


        let setExclMinAbsBSATotalSubstanceDosage = substanceDosageSetter Dosage.exclMinAbsBSATotalDosagePrism

        
        let getInclMaxAbsBSATotalSubstanceDosage = substanceDosageGetter Dosage.inclMaxAbsBSATotalDosagePrism


        let setInclMaxAbsBSATotalSubstanceDosage = substanceDosageSetter Dosage.inclMaxAbsBSATotalDosagePrism
        
        
        let getExclMaxAbsBSATotalSubstanceDosage = substanceDosageGetter Dosage.exclMaxAbsBSATotalDosagePrism


        let setExclMaxAbsBSATotalSubstanceDosage = substanceDosageSetter Dosage.exclMaxAbsBSATotalDosagePrism
    

    
    module Operators =
        
        let (|>>) (x1, x2) f = f x2 x1 

 
        let (|>>>) (x1, x2) f = f x2 x1, x2 


 
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

    let mdShapeText = """
Vorm: {shape}
"""

    let mdPatientText = """
{patient}
"""

    let mdDosageText = """
{dosage}
"""


    let toString (dr : DoseRule) =
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
                let ind = 
                    id.Indications 
                    |> String.concat ", "

                id.RouteDosages
                |> List.fold (fun acc rd -> 

                    rd.ShapeDosages
                    |> List.fold (fun acc sd ->

                        sd.PatientDosages
                        |> List.fold (fun acc pd ->

                            let s =
                                (mdPatientText
                                 |> String.replace "{patient}" (pd.Patient |> Patient.toString)) +
                                ("{dosage}" 
                                 |> String.replace "{dosage}" (pd.ShapeDosage |> Dosage.toString))
                            
                            pd.SubstanceDosages
                            |> List.fold (fun acc sd ->

                                acc + (mdDosageText |> String.replace "{dosage}" (sd |> Dosage.toString))

                            ) (acc + s)

                        ) (acc + (mdShapeText |> String.replace "{shape}" (sd.Shape |> String.concat ",")))
                                                
                    ) (acc + (mdRouteText |> String.replace "{route}" rd.Route))

                ) (acc + (mdIndicationText |> String.replace "{indication}" ind))
            ) s
        )




module GPP = Informedica.GenProduct.Lib.GenPresProduct
module ATC = Informedica.GenProduct.Lib.ATCGroup
module DR = Informedica.GenProduct.Lib.DoseRule
module RF = Informedica.GenProduct.Lib.RuleFinder

module ValueUnit = Informedica.GenUnits.Lib.ValueUnit

type GenPresProduct = GPP.GenPresProduct

type DoseRule = DoseRule.DoseRule
type IndicationDosage = DoseRule.IndicationDosage
type RouteDosage = DoseRule.RouteDosage
type ShapeDosage = DoseRule.ShapeDosage
type PatientDosage = DoseRule.PatientDosage
type SubstanceDosage = Dosage.Dosage
type Patient = Patient.Patient
type Dosage = Dosage.Dosage

type FlatDoseRule =
    {
        DoseRule : DoseRule
        IndicationDosage : IndicationDosage
        RouteDosage : RouteDosage
        ShapeDosage : ShapeDosage
        PatientDosage : PatientDosage
    }


/// Map GSTand min max float Option values to
/// a `DoseRule` `MinMax`
let mapMinMax<'a> 
              (setMin : float Option -> 'a -> 'a) 
              (setMax : float Option -> 'a -> 'a) 
              (minmax : DR.MinMax)
              (o : 'a) =
    o
    |> setMin minmax.Min
    |> setMax minmax.Max

 
/// Make sure that a GSTand time string 
/// is a valid unit time string
let parseTimeString s =
    s
    |> String.replace "per " ""
    |> String.replace "dagen" "dag"
    |> String.replace "weken" "week"
    |> String.replace "maanden" "maand"
    |> String.replace "minuten" "minuut"
    |> String.replace "uren" "uur"
    |> String.replace "eenmalig" ""
    |> (fun s -> 
        if s |> String.isNullOrWhiteSpace then s
        else s + "[Time]"
    )


/// Map a GStand time period to a valid unit
let mapTime s =
    s
    |> parseTimeString
    |> ValueUnit.Units.fromString


/// Map GStand frequency string to a valid 
/// frequency `ValueUnit`.
let mapFreq (fr: DR.Frequency) =
    let s = fr.Frequency |> string
    let s = s + " X[Count]"

    fr.Time
    |> parseTimeString
    |> (fun s' -> 
        match s' |> String.split " " with
        | [v;u] -> s + "/" + v + " " + u
        | [u]   -> 
            if u |> String.isNullOrWhiteSpace then s
            else 
                s + "/1" + " " + u
        | _ -> ""
    )
    |> ValueUnit.fromString


/// Map GSTand doserule doses to 
/// - normal   min max dose
/// - absolute min max dose
/// - normal   min max dose per kg
/// - absolute min max dose per kg
/// - normal   min max dose per m2
/// - absolute min max dose per m2
/// by calculating 
/// - substance shape concentration * dose shape quantity * frequency
/// for each dose
let mapDoses n qty unit (gstdsr : DR.DoseRule) =

    let fr = mapFreq gstdsr.Freq

    let setMin = Optic.set MinMax.Optics.inclMinLens
    let setMax = Optic.set MinMax.Optics.exclMaxLens

    let toVu perKg perM2 v =
        let u = 
            if not perKg then unit
            else
                unit |> ValueUnit.per ValueUnit.Units.Weight.kiloGram

        let u = 
            if not perM2 then u
            else
                u |> ValueUnit.per ValueUnit.Units.BSA.M2

        match u |> ValueUnit.fromFloat (v * qty) with
        | Some vu -> vu * fr |> Some
        | None -> None
    
    let minmax perKg perM2 (mm : DR.MinMax) =
        MinMax.empty
        |> setMin (mm.Min |> Option.bind (toVu perKg perM2))
        |> setMax (mm.Max |> Option.bind (toVu perKg perM2))

    (n, gstdsr.Freq.Time |> parseTimeString) ,
    (gstdsr.Indication,
     fr,
     gstdsr.Norm   |> minmax false false,
     gstdsr.Abs    |> minmax false false,
     gstdsr.NormKg |> minmax true false,
     gstdsr.AbsKg  |> minmax true false,
     gstdsr.NormM2 |> minmax false true,
     gstdsr.AbsM2  |> minmax false true)



let getSubstanceDoses (drs : DR.DoseRule seq) =
    drs 
    |> Seq.collect (fun dr ->
        dr.GenericProduct
        |> Seq.collect (fun gp ->
            gp.Substances
            |> Seq.collect (fun s ->
                match s.Unit |> ValueUnit.unitFromString Mapping.GStandMap with
                | None -> []
                | Some u ->
                      
                    [ mapDoses s.Name s.Quantity u dr ]
            )
        )
    )
    |> Seq.groupBy fst
    |> Seq.map (fun (k, v) -> 
        k , 
        v 
        |> Seq.fold (fun acc (_, (ind, fr, norm, abs, normKg, absKg, normM2, absM2)) -> 
            let inds, frs, norm_, abs_, normKg_, absKg_, normM2_, absM2_ = acc

            let frs =
                if frs |> List.exists ((=) fr) then frs else frs @ [ fr ]

            let inds =
                if inds |> List.exists ((=) ind) then inds else inds @ [ ind ]

            let norm = [ norm; norm_ ] |> MinMax.foldMaximize
            let abs = [ abs; abs_ ] |> MinMax.foldMaximize
            let normKg = [ normKg; normKg_ ] |> MinMax.foldMaximize
            let absKg = [ absKg; absKg_ ] |> MinMax.foldMaximize
            let normM2 = [ normM2; normM2_ ] |> MinMax.foldMaximize
            let absM2 = [ absM2; absM2_ ] |> MinMax.foldMaximize

            inds, frs, norm, abs, normKg, absKg, normM2, absM2
        ) ([], [], MinMax.empty, MinMax.empty, MinMax.empty, MinMax.empty, MinMax.empty, MinMax.empty)
    )
    |> Seq.map (fun (k, (inds, frs, norm, abs, normKg, absKg, normM2, absM2)) ->
        k ,
        inds ,
        frs ,
        DoseRange.create 
            norm 
            (normKg, ValueUnit.Units.Weight.kiloGram) 
            (normM2, ValueUnit.Units.BSA.M2)
            abs 
            (absKg, ValueUnit.Units.Weight.kiloGram) 
            (absM2, ValueUnit.Units.BSA.M2)
    )
    |> Seq.map (fun ((n, time), inds, frs, dr)  ->
        let tu = 
            match frs with
            | fr::_ -> fr |> ValueUnit.get |> snd
            | _ -> ValueUnit.NoUnit

        inds ,
        Dosage.empty
        |> (Optic.set  Dosage.Name_ n)  
        |> (fun ds ->
            match time with
            | _ when time |> String.isNullOrWhiteSpace -> 
                ds 
                |> (Optic.set Dosage.SingleDosage_ dr) 
            | _  ->
                ds
                |> (Optic.set Dosage.TotalDosage_ (dr, tu))
        )
    ) 


let getPatients (drs : DR.DoseRule seq) =
    let map = mapMinMax<Patient> 

    let ageInMo = Option.bind ValueUnit.ageInMo

    let wghtKg = Option.bind ValueUnit.weightInKg

    let mapAge = 
        map (ageInMo >> Patient.Optics.setInclMinAge) 
            (ageInMo >> Patient.Optics.setExclMaxAge) 

    let mapWght =
        map (wghtKg >> Patient.Optics.setInclMinWeight) 
            (wghtKg >> Patient.Optics.setInclMaxWeight) 
        
    drs
    |> Seq.map (fun dr ->
        (dr.Indication ,
         Patient.empty
         |> mapAge dr.Age
         |> mapWght dr.Weight) , dr
    )
    |> Seq.groupBy fst
    |> Seq.map (fun (k, v) -> k |> snd, v |> Seq.map snd)
    |> Seq.map (fun (pat, drs) ->
        (pat, drs |> getSubstanceDoses)
    )




// Get the ATC codes for a GenPresProduct
let getATCs (gpp : GenPresProduct) =
    gpp.GenericProducts
    |> Array.map(fun gp -> gp.ATC)
    |> Array.distinct

// Get the list of routes for a GenPresProduct
let getRoutes (gpp: GenPresProduct) = 
    gpp.GenericProducts
    |> Array.collect (fun gp -> gp.Route)
    |> Array.distinct

// Get the list of ATC groups for a GenPresProduct
let getATCGroups (gpp: GenPresProduct) =
    ATC.get ()
    |> Array.filter (fun g -> 
        gpp
        |> getATCs
        |> Array.exists (fun a -> 
            a |> String.equalsCapInsens g.ATC5) && g.Shape = gpp.Shape
        )
    |> Array.distinct


// Get the doserules for a genpresproduct
// ToDo Temp hack ignore route and shape
let getDoseRules (gpp : GenPresProduct) =
    gpp.Route
    |> Seq.collect (fun r ->
        RF.createFilter None None None None gpp.Name gpp.Shape r
        |> RF.find
        |> Seq.map (fun dr -> dr.Indication, (r, dr))
    )
    |> Seq.groupBy fst


   
let createDoseRules n =
    GPP.filter n "" ""
    |> Seq.collect (fun gpp ->
        gpp 
        |> getATCGroups
        |> Seq.map (fun atc -> 
            (atc.Generic, atc.ATC5, atc.TherapeuticMainGroup, atc.TherapeuticSubGroup, atc.PharmacologicalGroup, atc.Substance) ,
            gpp
        )
    )
    |> Seq.groupBy fst
    |> Seq.map (fun (k, v) ->
        let gen, atc, tg, tsg, pg, sg = k
        
        DoseRule.create gen atc tg tsg pg sg [] ,
        v
        |> Seq.map snd
    )
    |> Seq.map (fun (dr, gpps) ->
        dr ,
        gpps
        |> Seq.collect (fun gpp ->
            gpp.Route
            |> Seq.collect (fun r ->
                RF.createFilter None None None None gpp.Name gpp.Shape r
                |> RF.find
                |> getPatients
                |> Seq.collect (fun (pat, sds) ->
                    sds
                    |> Seq.map (fun (ind, sds) -> ind, r, gpp.Shape, pat, sds)
                )
            )
        )
        |> Seq.groupBy (fun (inds, _, _, _, _)  -> inds)
        |> Seq.map (fun (k, v) -> 
            k, 
            v
            |> Seq.map (fun (_, r, shp, pat, sds) -> r, shp, pat, sds)
            |> Seq.groupBy (fun (r, _, _, _)  -> r)
            |> Seq.map (fun (k, v) -> 
                k, 
                v
                |> Seq.map (fun (_, shp, pat, sds) -> shp, pat, sds)
                |> Seq.groupBy (fun (shp, _, _)  -> shp)
                |> Seq.map (fun (k, v) -> 
                    k, 
                    v
                    |> Seq.map (fun (_, pat, sds) -> pat, sds)
                )
            )
        )
    )
    |> Seq.map (fun (dr, inds) ->
        inds
        |> Seq.fold (fun acc ind ->
            let ind, _ = ind
            acc
            |> DoseRule.addIndications ind
        ) dr
    )


createDoseRules "paracetamol"
|> Seq.iter (fun dr ->
    dr 
    |> DoseRule.toString
    |> printfn "%s\n"
)




RF.createFilter None None None None "paracetamol" "" ""
|> RF.find
|> getSubstanceDoses
|> Seq.iter (fun (inds, sd) -> 
    printfn "Indication %s" (inds |> String.concat ", ")
    printfn "%s" (sd |> Dosage.toString)
)
 



RF.createFilter None None None None "paracetamol" "" ""
|> RF.find
|> getPatients
|> Seq.iter (fun (pat, sds) -> 
    printfn "Patient %s" (pat |> Patient.toString)
    sds
    |> Seq.iter (fun (inds, sd) -> 
    printfn "Indication %s" (inds |> String.concat ", ")
    printfn "%s" (sd |> Dosage.toString)
    )
)
