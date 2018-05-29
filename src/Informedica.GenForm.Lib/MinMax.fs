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


    let isValid ste min max = ste min max


    let createMinAndMaxDef ste def min max = 
        if isValid ste min max then (min, max) |> MinAndMax
        else def


    let createMinAndMax ste min max = createMinAndMaxDef ste none min max


    let setMinCond ste cond min minmax =
        match minmax with
        | MinAndMax (m, max) ->
            if cond min m then createMinAndMaxDef ste minmax min max else minmax
        | Max max -> createMinAndMaxDef ste minmax min max
        | None    -> createMin min
        | Min m   -> if cond min m then createMin min else minmax


    let setMaxCond ste cond max minmax =
        match minmax with
        | MinAndMax (min, m) ->
            if cond max m then createMinAndMaxDef ste minmax min max else minmax    
        | Min min -> createMinAndMaxDef ste minmax min max
        | None    -> createMax max
        | Max m   -> if cond max m then createMax max else minmax


    let setMin ste min minmax = setMinCond ste (fun _ _ -> true) min minmax


    let setMax ste max minmax = setMaxCond ste (fun _ _ -> true) max minmax


    let setSmallerMin ste st min minmax = setMinCond ste st min minmax


    let setLargerMax ste lt max minmax = setMaxCond ste lt max minmax


    let setLargerMin ste lt min minmax = setMinCond ste lt min minmax


    let setSmallerMax ste st max minmax = setMaxCond ste st max minmax
    

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


    let foldCond ste cond mms =
        let condMax m1 m2 = cond m2 m1
        mms |> List.fold (fun acc mm ->
            match mm with
            | None    -> acc
            | Min min -> setMinCond ste cond min acc
            | Max max -> setMaxCond ste condMax max acc
            | MinAndMax (min, max) ->
                acc
                |> setMinCond ste cond min
                |> setMaxCond ste condMax max
        ) none
     

    let foldMinimize ste gt = foldCond ste gt
     

    let foldMaximize ste st = foldCond ste st


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

