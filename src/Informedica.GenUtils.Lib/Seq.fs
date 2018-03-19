namespace Informedica.GenUtils.Lib

module Seq =

    let pickSeq pl (xs: 'a seq) =
        match pl with
        | [] -> xs
        | _ -> seq { for i in pl -> xs |> Seq.item i }

