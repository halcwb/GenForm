namespace Informedica.GenUtils.Lib.BCL

/// Helper functions for `System.String`
//open System.Security.Cryptography
module String = 

    open System
    open Informedica.GenUtils.Lib

    /// Apply `f` to string `s`
    let apply f (s: String) = f s
    
    /// Utility to enable type inference
    let get = apply id

    /// Split string `s` at character `c`
    let splitAt c s = 
        s |> NullCheck.nullOrDef (fun s' -> (s' |> get).Split([|c|])) [||] 
        

    /// Check if string `s2` contains string `s1`
    let contains= 
        NullCheck.nullOrDef2 (fun s1 s2 -> (s2 |> get).Contains(s1)) false

    /// Trim string `s`
    let trim= 
        NullCheck.nullOrDef (fun s -> (s |> get).Trim()) ""

    /// Make string all lower chars
    let toLower = 
        NullCheck.nullOrDef (fun s -> (s |> get).ToLower()) ""

    /// Make string all upper chars
    let toUpper = 
        NullCheck.nullOrDef (fun s -> (s |> get).ToUpper()) ""

    /// Get the length of s
    let length = 
        NullCheck.nullOrDef (fun s -> (s |> get).Length) 0

    /// Check if string is null or only white space
    let isNullOrWhiteSpace = String.IsNullOrWhiteSpace

    /// Check if string is null or only white space
    let empty s = String.IsNullOrWhiteSpace(s)

    /// Check if string is not null or only white space
    let notEmpty = empty >> not

    /// Replace `os` with `ns` in string `s`.
    let replace =
        NullCheck.nullOrDef3 (fun os ns s -> (s |> get).Replace(os, ns)) ""

    /// Convert object to string
    let toString o = 
        o |> NullCheck.nullOrDef (fun o' ->  o'.ToString()) ""

    /// Get a substring starting at `start` with length `length`
    let substring start length = 
        let sub s =
            if start < 0 || s |> String.length < start + length || start + length < 0  then ""
            else
                let s' = if length < 0 then start + length else start
                let l' = if length < 0 then -1 * length else length
                s.Substring(s', l')
        NullCheck.nullOrDef sub ""

    /// Get the first character of a string
    /// as a string
    let firstStringChar = substring 0 1

    /// Return the rest of a string as a string
    let restString s = 
        if s = "" then ""
        else
            substring 1 ((s |> length) - 1) s

    /// Make the first char of a string upper case
    let firstToUpper = firstStringChar >> toUpper

    /// Make the first character upper and the rest lower of a string
    let capitalize s = 
        if s = "" then ""
        else
            (s |> firstToUpper) + (s |> restString |> toLower)

    /// Get all letters as a string list
    let letters = 
        ['a'..'z'] @ ['A'..'Z'] 
        |> List.map string

    /// Check if a string is a letter
    let isLetter s = List.exists (fun s' -> s' = s) letters

    let equals s1 s2 = s1 = s2

    /// Check if string `s1` equals `s2` caps insensitive
    let equalsCapInsens s1 s2 = s1 |> toLower |> trim = (s2 |> toLower |> trim) 

    /// Split a string `s` at string `dels`
    let split (dels: string) (s: string) = 
        NullCheck.nullOrDef2 (fun (s': string) (dels': string) -> s'.Split(dels'.ToCharArray()) |> Array.toList) [] s dels

    /// Check whether **s1** starts with
    /// **s2** using string comparison **eqs**
    let startsWithEqs eqs s2 s1 =
        let sw s1 s2 =
            if s2 |> String.length > (s1 |> String.length) then false
            else
                s1 |> substring 0 (s2 |> String.length) |> eqs s2
        NullCheck.nullOrDef2 sw false s1 s2

    /// Check whether **s1** starts with
    /// **s2** caps sensitive
    let startsWith = startsWithEqs equals

    /// Check whether **s1** starts with
    /// **s2** caps insensitive
    let startsWithCapsInsens = startsWithEqs equalsCapInsens