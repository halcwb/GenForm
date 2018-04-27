module Tests

open Expecto

open Informedica.GenUnits.Lib

[<Tests>]
let tests =

    let rec testFind (ul : Unit.Units.UnitDetails List) =
        match ul with
        | [] -> Expect.isTrue true "units were found"
        | h::tail -> 
            match Unit.fromString (h.Name.Eng) (h.Group) with
            | Some _ -> testFind tail
            | None   -> Expect.isTrue false "unit could not be found"

    testList "Unit" [
        test "each unit in the list of units can be found" {
            Unit.Units.units 
            |> testFind
        }
        
    ]