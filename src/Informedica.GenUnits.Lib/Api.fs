namespace Informedica.GenUnits.Lib


module Api =

    open Informedica.GenUtils.Lib.BCL

    module VU = ValueUnit

    let eval s = 
        let addSpace s = " " + s + " "
        let mults  = "*" |> addSpace
        let divs   = "/" |> addSpace
        let adds   = "+" |> addSpace
        let subtrs = "-" |> addSpace 

        let del = "#"
        let addDel s = del + s + del

        let opts s = 
            let s = s |> String.trim
            match s with
            | _ when s = "*" -> (*)
            | _ when s = "/" -> (/)
            | _ when s = "+" -> (+)
            | _ when s = "-" -> (-)
            | _ -> failwith <| sprintf "Cannot evaluate string %s" s

        let rec eval' acc terms =
            if acc |> Option.isNone then 
                eval' (terms |> List.head |> VU.fromString |> Some) (terms |> List.tail)
            else
                match terms with
                | [] -> acc |> Option.get
                | os::vus::rest ->
                    let op = os |> opts
                    let vu = vus |> VU.fromString
                    rest |> eval' ((acc |> Option.get) |> op <| vu |> Some) 
                | _ -> failwith <| sprintf "Cannot evaluate string %s" (terms |> String.concat ",")      

        s 
        |> String.replace mults  (mults  |> addDel)
        |> String.replace divs   (divs   |> addDel)
        |> String.replace adds   (adds   |> addDel)
        |> String.replace subtrs (subtrs |> addDel)
        |> String.split del
        |> eval' None
        |> VU.toString VU.Units.English VU.Units.Verbal.Short



