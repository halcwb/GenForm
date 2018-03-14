namespace Informedica.GenUtils.Lib

/// Additional utitilty functions
/// for lists
module List =
    
    /// Replace an element **x** in a list **xs**
    /// when the **pred** function returns `true`. </br>
    /// Note: will only replace the *first* element
    /// that satisfies the condition in **pred**
    let replace pred x xs =
        match xs |> List.tryFindIndex pred with
        | Some(ind) ->
            (xs |> Seq.take ind |> Seq.toList) @ [x] @ 
            (xs |> Seq.skip (ind + 1) |> Seq.toList)
        | None -> xs

    /// Calling distinct on a list **xs**
    let distinct xs = xs |> Seq.ofList |> Seq.distinct |> Seq.toList


