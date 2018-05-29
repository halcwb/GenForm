namespace Informedica.GenForm.Lib


module MinMax =

    open Informedica.GenUtils.Lib


    type MinMax<'a> = 
        | None
        | Min of 'a
        | Max of 'a
        | MinAndMax of ('a * 'a)


    let none = None


    let createMin = Min


    let createMax = Max


    let isValid min max = min <= max


    let createMinAndMaxDef def min max = 
        if isValid min max then (min, max) |> MinAndMax
        else def


    let createMinAndMax min max = createMinAndMaxDef none min max


    let setMinCond cond min minmax =
        match minmax with
        | MinAndMax (m, max) ->
            if cond min m then createMinAndMaxDef minmax min max else minmax
        | Max max -> createMinAndMaxDef minmax min max
        | None    -> createMin min
        | Min m   -> if cond min m then createMin min else minmax


    let setMaxCond cond max minmax =
        match minmax with
        | MinAndMax (min, m) ->
            if cond max m then createMinAndMaxDef minmax min max else minmax    
        | Min min -> createMinAndMaxDef minmax min max
        | None    -> createMax max
        | Max m   -> if cond max m then createMax max else minmax


    let setMin min minmax = setMinCond (fun _ _ -> true) min minmax


    let setMax max minmax = setMaxCond (fun _ _ -> true) max minmax


    let setSmallerMin min minmax = setMinCond (<) min minmax


    let setLargerMax max minmax = setMaxCond (>) max minmax


    let setLargerMin min minmax = setMinCond (>) min minmax


    let setSmallerMax max minmax = setMaxCond (<) max minmax
    

    let apply fnone fmin fmax fmm minmax =
        match minmax with
        | None      -> None |> fnone
        | Min min   -> min  |> fmin
        | Max max   -> max  |> fmax
        | MinAndMax (min, max) ->  (min, max) |> fmm
    

    let getMin minmax = 
        let get (min, _) = min |> Some
        minmax |> apply Option.none Some Option.none get
    

    let getMax minmax = 
        let get (_,max) = max |> Some
        minmax |> apply Option.none Option.none Some get


    let getMinAndMax minmax = minmax |> apply Option.none Option.none Option.none Some


    let foldCond cond mms =
        let condMax m1 m2 = cond m2 m1
        mms |> List.fold (fun acc mm ->
            match mm with
            | None    -> acc
            | Min min -> setMinCond cond min acc
            | Max max -> setMaxCond condMax max acc
            | MinAndMax (min, max) ->
                acc
                |> setMinCond cond min
                |> setMaxCond condMax max
        ) none
     

    let foldMinimize gt mms = foldCond gt mms
     

    let foldMaximize st mms = foldCond st mms


    let inRange (gte: 'b -> 'b -> bool) (ste : 'b -> 'b -> bool) n minmax =
        match minmax with
        | None    -> true
        | Min min -> gte n min
        | Max max -> ste n max
        | MinAndMax (min, max) -> gte n min && ste n max


    let toString toStr minmax = 
        let s =
            match minmax with
            | None -> ""
            | Min min -> "vanaf " + (min |> toStr)
            | Max max ->
                "tot " + (max |> toStr)
            | MinAndMax (min, max) -> (min |> toStr) + " - " + (max |> toStr)

        if s = "" then "" else s + " "

