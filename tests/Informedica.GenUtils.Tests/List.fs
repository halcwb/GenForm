module ListTests

open Expecto

open Informedica.GenUtils.Lib

[<Tests>]
let tests =

    let equals exp txt res = Expect.equal res exp txt

    testList "List" [

        test "replace an element in an empty list " {
            []
            |> List.replace ((=) "a") ""
            |> equals [] "returns an empty list"
        }

        test "replace an element in a list with the element " {
            ["a"]
            |> List.replace ((=) "a") "b"
            |> equals ["b"] "returns the list with the first match replaced"
        }

        test "replace an element in a list without the element " {
            ["a"]
            |> List.replace ((=) "b") ""
            |> equals ["a"] "returns the list with the first match replaced"
        }

        test "replace an element in a list with multiple elements " {
            ["a";"b";"a"]
            |> List.replace ((=) "a") "b"
            |> equals ["b";"b";"a"] "returns the list with the first match replaced"
        }


    ]
