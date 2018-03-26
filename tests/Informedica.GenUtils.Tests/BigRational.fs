module BigRationalTests

open Expecto

open MathNet.Numerics
open Informedica.GenUtils.Lib.BCL

[<Tests>]
let tests =

    let equals exp txt res = Expect.equal res exp txt

    let config = { FsCheckConfig.defaultConfig with maxTest = 10000 }

    let opMult f () = f (*)

    testList "BigRational" [

        test "can parse a string number 1" {
            "1"
            |> BigRational.tryParse
            |> equals (Some 1N) "to a br 1"
        }

        testPropertyWithConfig config "parse can be reversed" <| fun a ->
            match a |> BigRational.tryParse with
            | Some b -> 
                b 
                |> BigRational.toString 
                |> BigRational.parse = b
            | None -> true

        testPropertyWithConfig config "when a is gcd of b and c then b and c both are a multiple of a" <| fun n1 d1 n2 d2 ->
            let b = ((if n1 = 0 then 1 else n1) |> BigRational.fromInt) / ((if d1 = 0 then 1 else d1) |> BigRational.fromInt)
            let c = ((if n2 = 0 then 1 else n2) |> BigRational.fromInt) / ((if d2 = 0 then 1 else d2) |> BigRational.fromInt)
            let a = BigRational.gcd b c
            // printfn "%s %s %s" (b |> BigRational.toString) (c |> BigRational.toString) (a |> BigRational.toString)
            b |> BigRational.isMultiple a && c |> BigRational.isMultiple a


        testPropertyWithConfig config "when b is converted to multiple of c then result a is multiple of c" <| fun n1 d1 n2 d2 ->
            let b = ((if n1 = 0 then 1 else n1) |> BigRational.fromInt) / ((if d1 = 0 then 1 else d1) |> BigRational.fromInt)
            let c = ((if n2 = 0 then 1 else n2) |> BigRational.fromInt) / ((if d2 = 0 then 1 else d2) |> BigRational.fromInt)
            let a = b |> BigRational.toMultipleOf c
            // printfn "%s %s %s" (b |> BigRational.toString) (c |> BigRational.toString) (a |> BigRational.toString)
            a |> BigRational.isMultiple c 


        test "when operator is multiplication" {
            Expect.equal ((*) |> BigRational.opIsMult) true ""
        }

    ]
