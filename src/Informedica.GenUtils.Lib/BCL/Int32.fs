namespace Informedica.GenUtils.Lib.BCL
    
module Int32 =

    open System

    let parse s = Int32.Parse(s, Globalization.NumberStyles.Any, Globalization.CultureInfo.InvariantCulture)

    let tryParse s =
        let (b, n) = Int32.TryParse(s, Globalization.NumberStyles.Any, Globalization.CultureInfo.InvariantCulture)
        if b then n |> Some else None