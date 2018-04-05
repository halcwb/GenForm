module BigRationalTests

open Expecto

open MathNet.Numerics
open Informedica.GenUtils.Lib.BCL


/// Create the necessary test generators
module Generators =

    open FsCheck

    let bigRGen (n, d) = 
            let d = if d = 0 then 1 else d
            let n' = abs(n) |> BigRational.FromInt
            let d' = abs(d) |> BigRational.FromInt
            n'/d'

    let bigRGenerator =
        gen {
            let! n = Arb.generate<int>
            let! d = Arb.generate<int>
            return bigRGen(n, d)
        }

    type MyGenerators () =
        static member BigRational () =
            { new Arbitrary<BigRational>() with
                override x.Generator = bigRGenerator }


[<Tests>]
let tests =

    let equals exp txt res = Expect.equal res exp txt

    let config = 
        { FsCheckConfig.defaultConfig with 
            maxTest = 10000
            arbitrary = [ typeof<Generators.MyGenerators> ] }

    let opMult f () = f (*)

    testList "BigRational" [

        test "can parse a string number 1" {
            "1"
            |> BigRational.tryParse
            |> equals (Some 1N) "to a br 1"
        }

        testPropertyWithConfig config "can try to convert any double to bigrational" <| fun (a: float) ->
            a 
            |> (BigRational.fromFloat >> Option.defaultValue 0N >> BigRational.toFloat) 
            |> (fun b -> 
                if b = 0. || Accuracy.areClose Accuracy.veryHigh a b then true
                else
                    printfn "%f <> %f" a b
                    false
            )


        testPropertyWithConfig config "can convert any bigrational to a double" <| fun br ->
            let f = 
                br 
                |> BigRational.toFloat
            f
            |> BigRational.fromFloat 
            |> (fun r -> 
                if r |> Option.isNone then false
                else
                    r
                    |> Option.get
                    |> BigRational.toFloat
                    |> Accuracy.areClose Accuracy.veryHigh f
            )

        testPropertyWithConfig config "can parse any string float" <| fun (a: float) ->
            match a |> (BigRational.fromFloat >> Option.defaultValue 0N >> string >> BigRational.tryParse) with
            | Some b -> 
                b
                |> BigRational.toString 
                |> BigRational.parse = b
            | None -> true

        testPropertyWithConfig config "parse can be reversed" <| fun a ->
            match a |> BigRational.tryParse with
            | Some b -> 
                b 
                |> BigRational.toString 
                |> BigRational.parse = b
            | None -> true

        testPropertyWithConfig config "when a is gcd of b and c then b and c both are a multiple of a" <| fun b c ->
            // printfn "%s %s %s" (b |> BigRational.toString) (c |> BigRational.toString) (a |> BigRational.toString)
            if (b = 0N || c = 0N) then true
            else
                let a = BigRational.gcd b c
                b |> BigRational.isMultiple a && 
                c |> BigRational.isMultiple a


        testPropertyWithConfig config "when b is converted to multiple of c then result a is multiple of c" <| fun b c ->
            // printfn "%s %s %s" (b |> BigRational.toString) (c |> BigRational.toString) (a |> BigRational.toString)
            if (b = 0N || c = 0N) then true
            else
                let a = b |> BigRational.toMultipleOf c
                a |> BigRational.isMultiple c 

        testPropertyWithConfig config "can check is multiple for any bigrational" <| fun b c ->
            if c = 0N then b |> BigRational.isMultiple c |> not
            else
                if b |> BigRational.isMultiple c then (b / c).Denominator = 1I
                else (b / c).Denominator <> 1I 

        test "when operator is multiplication" {
            Expect.equal ((*) |> BigRational.opIsMult) true ""
        }

        test "when operator is addition" {
            Expect.equal ((+) |> BigRational.opIsAdd) true ""
        }

        test "when operator is division" {
            Expect.equal ((/) |> BigRational.opIsDiv) true ""
        }

        test "when operator is subtraction" {
            Expect.equal ((-) |> BigRational.opIsSubtr) true ""
        }

    ]
