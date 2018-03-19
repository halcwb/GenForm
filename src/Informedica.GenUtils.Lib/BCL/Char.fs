namespace Informedica.GenUtils.Lib.BCL

module Char =

    let letters = [|'a'..'z'|]

    let capitals = [|'A'..'Z'|]

    let isCapital c = capitals |> Seq.exists ((=) c)
