namespace Informedica.GenUtils.Lib

module Continuation =

    let tryCatchCont f fsucc ffail x =
        try 
            f x
            |> fsucc
        with
        | exn -> (x, exn) |> ffail
    

    let nullCont f fsucc fnull x =
        match x |> f with
        | y when y |> isNull -> x |> fnull
        | y -> y |> fsucc


    let rec nullFix f ffix x = 
        match x |> f with
        | y when y |> isNull -> 
            x |> ffix
            x |> nullFix f ffix
        | y -> y 
