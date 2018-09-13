module Tests

open Expecto

open MathNet.Numerics

module VU = Informedica.GenUnits.Lib.ValueUnit


let toString = VU.toString VU.Units.English VU.Units.Short

let (>>*) u f = 
    u |> toString |> printfn "%s"
    f u

// Some basic units
let mg400 = 400N |> VU.create VU.Units.Mass.milliGram
let ml50  = 50N  |> VU.create VU.Units.Volume.milliLiter
let ml5  = 5N    |> VU.create VU.Units.Volume.milliLiter
let l5 = 5N      |> VU.create VU.Units.Volume.liter 

// The count group is a special unit group 
// with only one unit: times. 
let times3 = 3N |> VU.create VU.Units.Count.times
let times100 = 100N |> VU.create VU.Units.Count.times


[<Tests>]
let unitTests =


    testList "Unit" [
        test "base value of 400 mg = 0.4 g" {
            Expect.isTrue (mg400 |> VU.toBase = (400N/1000N)) ""
        }

        test "base value of 50 ml = 0.05 l" {
            Expect.isTrue (ml50 |> VU.toBase = (50N/1000N)) ""
        }

        test "base value of 5 ml = 0.005 l" {
            Expect.isTrue (ml5 |> VU.toBase = (5N/1000N)) ""
        }

        test "base value of 5 l = 5 l" {
            Expect.isTrue (l5 |> VU.toBase = (5N/1N)) ""
        }        
        
        test "count 3 times 5 ml results in 15 ml" {
            Expect.isTrue (ml5 * times3 |> VU.toBase = (3N * 5N) / 1000N) " "
        }
    ]


[<Tests>]
let comparisonTests =

    testList "Comparison" [

        test "ml50 < l5 using normal operator < should be false" {
            Expect.isFalse (ml50 < l5) ""
        }

        test "ml50 < l5 using special operator <?" {
            Expect.isTrue (ml50 <? l5) ""
        }

        test "ml50 = l5 should be false" {
            Expect.isFalse (ml50 =? l5) ""
        }

        test "ml50 * times 100 = l5 should be true" {
            Expect.isTrue (ml50 * times100  =? l5) ""
        }
    ]

[<Tests>]
let calculationTests =

    let (>>?) res exp =
        Expect.isTrue (res = (exp |> VU.fromString)) ""
        res


    testList "Calculation" [

        test "3 times 3 = 9" {
            Expect.isTrue (times3 * times3 |> VU.toBase = 9N) ""
        }        

        test "3 divided by 3 = 1" {
            Expect.isTrue (times3 / times3 |> VU.toBase = 1N) ""
        }        

        test "3 plus 3 = 6" {
            Expect.isTrue (times3 + times3 |> VU.toBase = 6N) ""
        }        

        test "3 minus 3 = 0" {
            Expect.isTrue (times3 - times3 |> VU.toBase = 0N) ""
        }

        test "can add or subrract within the same unit group" {
            Expect.isTrue ((l5 + ml50) >? l5) ""
            Expect.isTrue ((l5 - ml50) <? l5) ""
        }        

        test "cannot add or subrract with different unit groups" {
            Expect.throws (fun _ -> (l5 + mg400) >? l5 |> ignore) ""
            Expect.throws (fun _ -> (l5 - mg400) <? l5 |> ignore) ""
        }

        test "division by unit with the same unit group results in a count" {
            let (_, u) = (l5 / ml50) |> VU.get
            let g = u |> VU.Group.unitToGroup
            printfn "%A" g
            Expect.isTrue (g = VU.Group.CountGroup) ""
        }

        test "can calculate with units" {
            (mg400 + mg400)
            >>? "800 mg[Mass]"
            |> (fun vu -> vu / ml50)
            >>? "16 mg[Mass]/ml[Volume]"
            |> (fun vu -> vu * ml50)
            >>? "800 mg[Mass]"
            |> (fun vu -> vu - mg400)
            >>? "400 mg[Mass]"
            |> ignore    
        }

    ]