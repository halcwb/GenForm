module DoubleTests

open System
open Expecto
open MathNet.Numerics

open Informedica.GenUtils.Lib.BCL

[<Tests>]
let tests =

    let equals exp txt res = Expect.equal res exp txt

    let config = 
        { FsCheckConfig.defaultConfig with 
            maxTest = 10000 }

    testList "Double" [

        testPropertyWithConfig config "any valid string double can be parsed to a double" <| fun (a: Double) ->
            if a |> Double.isValid |> not then true
            else
                a
                |> string
                |> Double.parse
                |> string
                |> ((=) (string a))

        testPropertyWithConfig config "any string can be used to try parse" <| fun s ->
            s
            |> Double.tryParse
            |> (fun _ -> true)

        testPropertyWithConfig config "getPrecision can be calculated for any valid double" <| fun (a: Double) n ->
            if a |> Double.isValid |> not then true
            else
                a
                |> Double.getPrecision n
                |> (fun _ -> true)

        testPropertyWithConfig config "getPrecision for a abs value < 0 never returns a smaller value than precision (> 0)" <| fun (a: Double) n ->
            if n <= 0 || a |> Double.isValid |> not then true
            else
                a
                |> Double.getPrecision n
                |> (fun x -> 
                    if a |> abs < 0. && x < n then
                        printfn "decimals %i < precision %i for value %f" x n a
                        false
                    else true
                )

        testPropertyWithConfig config "getPrecision for every precision < 0 returns same as n = 0" <| fun (a: Double) n ->
            if a |> Double.isValid |> not then true
            else
                a
                |> Double.getPrecision n
                |> (fun x -> 
                    if n < 0 then
                        x = (a |> Double.getPrecision 0)
                    else true
                )

        // * 66.666 |> Double.getPrecision 1 = 0
        test "66.666 |> Double.getPrecision 1 = 0" {
            66.666 |> Double.getPrecision 1 
            |> equals 0 ""            
        }

        // * 6.6666 |> Double.getPrecision 1 = 0
        test "6.6666 |> Double.getPrecision 1 = 0" {
            6.6666 |> Double.getPrecision 1 
            |> equals 0 ""            
        }

        // * 0.6666 |> Double.getPrecision 1 = 1
        test "6.6666 |> Double.getPrecision 1 = 1" {
            0.6666 |> Double.getPrecision 1 
            |> equals 1 ""            
        }

        // * 0.0666 |> Double.getPrecision 1 = 2
        test "0.0666 |> Double.getPrecision 1 = 2" {
            0.0666 |> Double.getPrecision 1
            |> equals 2 ""            
        }

        // * 0.0666 |> Double.getPrecision 0 = 0
        test "0.0666 |> Double.getPrecision 0 = 0" {
            0.0666 |> Double.getPrecision 0
            |> equals 0 ""            
        }

        // * 0.0666 |> Double.getPrecision 2 = 3
        test "0.0666 |> Double.getPrecision 2 = 3" {
            0.0666 |> Double.getPrecision 2
            |> equals 3 ""            
        }

        // * 0.0666 |> Double.getPrecision 3 = 4
        test "0.0666 |> Double.getPrecision 3 = 4" {
            0.0666 |> Double.getPrecision 3
            |> equals 4 ""            
        }

        // * 6.6666 |> Double.getPrecision 0 = 0
        test "6.6666 |> Double.getPrecision 0 = 0" {
            6.6666 |> Double.getPrecision 0
            |> equals 0 ""            
        }

        // * 6.6666 |> Double.getPrecision 2 = 1
        test "6.6666 |> Double.getPrecision 2 = 1" {
            6.6666 |> Double.getPrecision 2
            |> equals 1 ""            
        }

        // * 6.6666 |> Double.getPrecision 3 = 2
        test "6.6666 |> Double.getPrecision 3 = 2" {
            6.6666 |> Double.getPrecision 3
            |> equals 2 ""            
        }


        // * 66.666 |> Double.fixPrecision 1 = 67
        test "66.666 |> Double.fixPrecision 1 = 67" {
            66.666 |> Double.fixPrecision 1 
            |> equals 67. ""            
        }

        // * 6.6666 |> Double.fixPrecision 1 = 7
        test "6.6666 |> Double.fixPrecision 1 = 7" {
            6.6666 |> Double.fixPrecision 1 
            |> equals 7. ""            
        }

        // * 0.6666 |> Double.fixPrecision 1 = 0.7
        test "6.6666 |> Double.fixPrecision 1 = 0.7" {
            0.6666 |> Double.fixPrecision 1 
            |> equals 0.7 ""            
        }

        // * 0.0666 |> Double.fixPrecision 1 = 0.07
        test "0.0666 |> Double.fixPrecision 1 = 0.07" {
            0.0666 |> Double.fixPrecision 1
            |> equals 0.07 ""            
        }

        // * 0.0666 |> Double.fixPrecision 0 = 0
        test "0.0666 |> Double.fixPrecision 0 = 0" {
            0.0666 |> Double.fixPrecision 0
            |> equals 0. ""            
        }

        // * 0.0666 |> Double.fixPrecision 2 = 0.067
        test "0.0666 |> Double.fixPrecision 2 = 0.067" {
            0.0666 |> Double.fixPrecision 2
            |> equals 0.067 ""            
        }

        // * 0.0666 |> Double.fixPrecision 3 = 0.0666
        test "0.0666 |> Double.fixPrecision 3 = 0.0666" {
            0.0666 |> Double.fixPrecision 3
            |> equals 0.0666 ""            
        }

        // * 6.6666 |> Double.fixPrecision 0 = 7
        test "6.6666 |> Double.fixPrecision 0 = 7" {
            6.6666 |> Double.fixPrecision 0
            |> equals 7. ""            
        }

        // * 6.6666 |> Double.fixPrecision 2 = 6.7
        test "6.6666 |> Double.fixPrecision 2 = 6.7" {
            6.6666 |> Double.fixPrecision 2
            |> equals 6.7 ""            
        }

        // * 6.6666 |> Double.fixPrecision 3 = 6.67
        test "6.6666 |> Double.fixPrecision 3 = 6.67" {
            6.6666 |> Double.fixPrecision 3
            |> equals 6.67 ""            
        }

        testPropertyWithConfig config "for any valid float, this float can be converted to a fraction" <| fun f ->
            if f |> Double.isValid |> not then true
            else
                f
                |> Double.floatToFract
                |> (fun r ->
                    match r with
                    | None -> true
                    | Some (n, d) -> 
                        ((n |> BigRational.FromBigInt) / (d |> BigRational.FromBigInt))
                        |> ((=) (f |> BigRational.fromFloat |> Option.get))
                )

    ]
    