namespace Informedica.GenForm.Lib.Utils


module List =

    open System

    open Informedica.GenUtils.Lib.BCL
    open Informedica.GenUtils.Lib

    let tryFindRest pred xs =
        let rec find x xs notx =
            match xs with
            | [] -> x, notx
            | h::tail ->
                if h |> pred then find (Some h) tail notx
                else find x tail ([h] |> List.append notx)

        find None xs []


    let inline toString xs =
        let toStr = Informedica.GenUtils.Lib.List.toString

        xs
        |> toStr
        |> String.replace "[" ""
        |> String.replace "]" ""
        |> String.replace ";" ","
    

    let prepend l1 l2 = List.append l2 l1

