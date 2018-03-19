module CombiUnitTests

open Expecto

open MathNet.Numerics
open Informedica.GenUnits.Lib


[<Tests>]
let tests =

    let equals exp txt res = Expect.equal res exp txt

    let toStringP cu = 
        let s = cu |> CombiUnit.toString
        printfn "%s" s
        s

    testList "Combiunit" [

        test "fromString can be reversed toString" {
            let cu = "mg[Mass]"
            cu
            |> CombiUnit.fromString
            |> CombiUnit.toString
            |> equals cu "mg [Mass]"
        }

        test "can be a combined unit" {
            let cu = "mg[Mass]/kg[Weight]/min[Time]"
            cu 
            |> CombiUnit.fromString
            |> CombiUnit.toString
            |> equals cu "should be the same"
        }

        test "each unit can be combined with another unit" {
            let cus =
                [ for g1 in Unit.Units.groups do
                    for g2 in Unit.Units.groups do
                        if g1 <> g2 then 
                            for u1 in (g1 |> Unit.Units.unitsFromGroup) do
                                for u2 in (g2 |> Unit.Units.unitsFromGroup) do
                                    let cu1 = u1 |> CombiUnit.create 1N
                                    let cu2 = u2 |> CombiUnit.create 1N
                                    
                                    yield! [ cu1/cu2 ] @ [ cu1 * cu2 ]
                        else yield! []
                ]
            cus 
            |> List.map (CombiUnit.toString >> CombiUnit.fromString)
            |> equals cus ""
        }
    ]
