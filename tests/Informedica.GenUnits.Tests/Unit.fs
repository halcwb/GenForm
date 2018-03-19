module Tests

open Expecto

open Informedica.GenUnits.Lib

[<Tests>]
let tests =

    let rec testFind (ul : Unit.Unit List) =
        match ul with
        | [] -> Expect.isTrue true "units were found"
        | h::tail -> 
            match Unit.Units.fromString (h.Name |> fst |> Unit.Name.toString) (h.Group |> Unit.Name.toString) with
            | Some _ -> testFind tail
            | None   -> Expect.isTrue false "unit could not be found"

    testList "Unit" [
        test "each unit in the list of units can be found" {
            Unit.Units.units 
            |> List.collect id
            |> testFind
        }
        
        test "for each unit group a list of units can be retrieved" {
            let rec fromGr gs =
                match gs with
                | [] -> Expect.isTrue true "no empty lists" 
                | g::tail -> 
                    if g |> Unit.Units.unitsFromGroup = List.empty then 
                        Expect.isTrue false "empty list"
                    else 
                        tail |> fromGr
            Unit.Units.groups
            |> fromGr
        }
    ]