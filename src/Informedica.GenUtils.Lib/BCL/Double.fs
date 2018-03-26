namespace Informedica.GenUtils.Lib.BCL

module Double =

    open System
    open System.Numerics

    open MathNet.Numerics

    let parse s = Double.Parse(s, Globalization.CultureInfo.InvariantCulture)

    let tryParse s =
        let (b, n) = Double.TryParse(s, Globalization.NumberStyles.Any, Globalization.CultureInfo.InvariantCulture) //Double.TryParse(s)
        if b then Some n else None

    let stringToFloat32 s =
        match s |> tryParse with
        | Some v -> float32 v
        | None -> 0.f

    /// Calculates the number of decimals that 
    /// should be shown according to a precision 
    /// number n that specifies the number of non
    /// zero digits in the decimals
    let getPrecision n f =
        if f = 0. then n
        else
            let f = (f |> string).Split([|'.'|])
            let n = n - if f.[0] = "0" then 0 else f.[0].Length
            let n = if n < 0 then 0 else n
            if (int f.[0]) > 0 then
                n
            else
                let c = (f.[1] |> String.countFirstChar '0')
                c + n

    /// Fix the precision of a float f to
    /// match a minimum of non zero digits n
    let fixPrecision n (f: float) =
        Math.Round(f, f |> getPrecision n)

    let floatHasDecimals (v: float) = 
        v > float(BigInteger v)

    let floatToFract v =
        let rec fract (v:float) m =
            match v |> floatHasDecimals with
            |false  -> (BigInteger(v) , m)
            |true -> fract (v * 10.) (m * 10N)
        fract v 1N