module ReflectionTests

open Expecto

open Informedica.GenUtils.Lib

type TestUnion = TestUnion | AnotherTestUnion

[<Tests>]
let tests =

  testList "Reflection toString and fromString " [

    testCase "of discriminate union TestUnion" <| fun _ ->
      Expect.equal (TestUnion |> Reflection.toString) "TestUnion" "should print TestUnion"
     
    test "of discriminate union AnotherTestUnion" {
      Expect.equal (AnotherTestUnion |> Reflection.toString) "AnotherTestUnion" "should print AnotherTestUnion"
    }

    test "can create a TestUnion Option" {
        Expect.equal ("TestUnion" |> Reflection.fromString<TestUnion>) (Some TestUnion) "from string TestUnion"
    }

    test "will return None with a non existing union type" {
        Expect.equal ("blah" |> Reflection.fromString<TestUnion>) None "from string blah"
    }
 
  ]
