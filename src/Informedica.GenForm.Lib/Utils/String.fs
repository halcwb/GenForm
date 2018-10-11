namespace Informedica.GenForm.Lib.Utils

module String =

    open Informedica.GenUtils.Lib.BCL


    let removeTextBetween start stop text =
        let regex = @"\" + start + @"[^\" + stop + "]*]"
        printfn "regex: %s" regex

        (String.regex regex).Replace(text, "")
        |> String.trim


    let removeTextBetweenBrackets = removeTextBetween "[" "]"
        
  

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