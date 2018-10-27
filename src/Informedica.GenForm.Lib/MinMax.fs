namespace Informedica.GenForm.Lib



module MinMax =

    open MathNet.Numerics
    open Informedica.GenForm.Lib.Utils

    open Aether
    open Aether.Operators
    

    module ValueUnit = Informedica.GenUnits.Lib.ValueUnit
     

    type ValueUnit = ValueUnit.ValueUnit


    let (<?) = ValueUnit.st
    let (>?) = ValueUnit.gt
    let (<=?) = ValueUnit.ste
    let (>=?) = ValueUnit.gte
    let (>>=) l r = ValueUnit.convertTo r l

    /// Range with min and/or max
    type MinMax =
        {
            Min : Value option
            Max : Value option
        }
    and Value = Inclusive of ValueUnit | Exclusive of ValueUnit


    let inclusive v = v |> Inclusive


    let exclusive v = v |> Exclusive


    let create min max = { Min = min; Max = max }
    

    let empty = create None None
    

    let inline applyValue1 f1 f2 v1 =
            match v1 with
            | Inclusive vu1 -> vu1 |> f1 |> Inclusive
            | Exclusive vu1 -> vu1 |> f2 |> Exclusive
    

    let inline applyValue2 f1 f2 f3 f4 v1 v2 =
            match v1, v2 with
            | Inclusive vu1, Inclusive vu2 -> vu1 |> f1 <| vu2
            | Inclusive vu1, Exclusive vu2 -> vu1 |> f2 <| vu2
            | Exclusive vu1, Inclusive vu2 -> vu1 |> f3 <| vu2
            | Exclusive vu1, Exclusive vu2 -> vu1 |> f4 <| vu2


    let valueLT = applyValue2 (>?) (>=?) (>?) (>?) 


    let valueST = applyValue2 (<?) (<?) (<=?) (<?) 


    let valueLTE = applyValue2 (>=?) (>=?) (>=?) (>=?)


    let valueSTE = applyValue2 (<=?) (<=?) (<=?) (<=?) 


    let isValid ({ Min = min; Max = max }) =
        match min, max with
        | None, None -> true
        | Some _, None | None, Some _ -> true
        | Some v1, Some v2 ->
            applyValue2 (<=?) (<?) (<?) (<?) v1 v2


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


    let calcValue op v1 v2 =
        match v1, v2 with
        | Inclusive v1, Inclusive v2 -> v1 |> op <| v2 |> Inclusive
        | Exclusive v1, Exclusive v2 -> v1 |> op <| v2 |> Exclusive
        | Inclusive v1, Exclusive v2 -> v1 |> op <| v2 |> Exclusive
        | Exclusive v1, Inclusive v2 -> v1 |> op <| v2 |> Exclusive  


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

        static member (*) (v1, v2) = calcValue (*) v1 v2

        static member (/) (v1, v2) = calcValue (/) v1 v2

    
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
        let vuToStr vu = 
            let milliGram = ValueUnit.Units.Mass.milliGram
            let gram = ValueUnit.Units.Mass.gram
            let day = ValueUnit.Units.Time.day

            let per = ValueUnit.per
            let convertTo = ValueUnit.convertTo

            let milliGramPerDay = milliGram |> per day
            let gramPerDay = gram |> per day

            vu
            |> (fun vu ->
                match vu |> ValueUnit.get with
                | (v, u) when v >= 1000N && u = milliGram -> vu |> convertTo gram
                | (v, u) when v >= 1000N && u = milliGramPerDay -> vu |> convertTo gramPerDay
                | _ -> vu
            )
            |> ValueUnit.toStringPrec 2

        let minToString min =
            match min with 
            | Inclusive vu ->
                vu |> vuToStr |> sprintf "%s"
            | Exclusive vu ->
                vu |> vuToStr |> sprintf "%s"

        let maxToString min =
            match min with 
            | Inclusive vu ->
                vu |> vuToStr |> sprintf "%s"
            | Exclusive vu ->
                vu |> vuToStr |> sprintf "%s"

        match min, max with
        | None, None -> ""
        | Some min_, Some max_ -> 
            sprintf "%s - %s" (min_ |> minToString) (max_ |> maxToString)
        | Some min_, None -> 
            (min_ |> minToString) 
            |> sprintf "vanaf %s"
        | None, Some max_ -> 
            (max_ |> maxToString)
            |> sprintf "tot %s"


    let ageToString { Min = min; Max = max } =
        let oneWk = 1N |> ValueUnit.create ValueUnit.Units.Time.week
        let oneMo = 1N |> ValueUnit.create ValueUnit.Units.Time.month
        let oneYr = 1N |> ValueUnit.create ValueUnit.Units.Time.year

        let convert = 
            let c vu =
                match vu with 
                | _ when vu <? oneWk -> vu >>= ValueUnit.Units.Time.day
                | _ when vu <? oneMo -> vu >>= ValueUnit.Units.Time.week
                | _ when vu <? oneYr -> vu >>= ValueUnit.Units.Time.month
                | _ -> vu >>= ValueUnit.Units.Time.year
            Option.bind (applyValue1 c c >> Some)
            
        { Min = min |> convert; Max = max |> convert } |> toString
        
        


    let gestAgeToString { Min = min; Max = max } =

        let convert = 
            let c vu = vu >>= ValueUnit.Units.Time.week
            Option.bind (applyValue1 c c >> Some)
            
        { Min = min |> convert; Max = max |> convert } |> toString        