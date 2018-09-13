module Tests

open Expecto

open MathNet.Numerics

module VU = Informedica.GenUnits.Lib.ValueUnit


let toString = VU.toString VU.Units.English VU.Units.Short

let (>>*) u f = 
    u |> toString |> printfn "%s"
    f u

[<Tests>]
let tests =

    // let rec testFind (ul : Unit.Units.UnitDetails List) =
    //     match ul with
    //     | [] -> Expect.isTrue true "units were found"
    //     | h::tail -> 
    //         match Unit.fromString (h.Name.Eng) (h.Group) with
    //         | Some _ -> testFind tail
    //         | None   -> Expect.isTrue false "unit could not be found"

    // Some basic units
    let mg400 = 400N |> VU.create VU.Units.Mass.milliGram
    let ml50  = 50N  |> VU.create VU.Units.Volume.milliLiter
    let ml5  = 5N    |> VU.create VU.Units.Volume.milliLiter
    let l5 = 5N      |> VU.create VU.Units.Volume.liter 

    // The count group is a special unit group 
    // with only one unit: times.
    let times3 = 3N |> VU.create VU.Units.Count.times

    testList "Unit" [
        test "base value of 400 mg = 0.4" {
            Expect.isTrue (mg400 |> VU.toBase = (4N/10N)) "This test must stil be written"
        }
        
    ]