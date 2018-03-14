namespace Informedica.GenUtils.Lib

/// Function to perform a safe null check
module NullCheck =

    /// This is the F# 4 implementation of
    /// checking whether a value is null.
    [<CompiledName("IsNull")>]
    let inline isNull (value : 'T) = 
        match value with 
        | null -> true 
        | _ -> false

    let nullOrDef f d v =
        if v |> isNull then d
        else v |> f

    let nullOrDef2 f d v1 v2 =
        if (v1 |> isNull) || (v2 |> isNull) then d
        else f v1 v2
        
    let nullOrDef3 f d v1 v2 v3 =
        if (v1 |> isNull) || (v2 |> isNull) || (v2 |> isNull) then d
        else f v1 v2 v3
        
