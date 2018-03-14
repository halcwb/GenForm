module StringTests

open Expecto

open Informedica.GenUtils.Lib.BCL

[<Tests>]
let tests =

    let equals exp txt res = Expect.equal res exp txt

    testList "String" [

        test "splitAt can split a string at character " {
            Expect.equal ("Hello World" |> String.splitAt ' ')  [|"Hello"; "World"|] " space "
        }

        test "splitAt split of null will yield "  {
            null
            |> String.splitAt ' '
            |> equals [||] " empty array "
        }

        test "splitAt split of an empty string will yield " {
            ""
            |> String.splitAt 'a'
            |> equals [|""|] "an array with an empty string"
        }

        test "split split ca split a string with a string" {
            "Hello_world"
            |> String.split "_"
            |> equals ["Hello"; "world"] "into a list of two strings"
        }

        test "split with a null will yield" {
            null
            |> String.split ""
            |> equals [] "an empty list"
        }

        test "capitalize of an empty string" {
            ""
            |> String.capitalize
            |> equals "" "returns an empty string"
        }

        test "capitalize of an null string" {
            null
            |> String.capitalize
            |> equals "" "returns an empty string"
        }    

        test "capitalize of hello world" {
            "hello world"
            |> String.capitalize
            |> equals "Hello world" "returns an empty string"
        }

        test "contains null string null string" {
            null
            |> String.contains null
            |> equals false "returns false"
        }

        test "contains empty string null string" {
            ""
            |> String.contains null
            |> equals false "returns false"
        }

        test "contains null string empty string" {
            null
            |> String.contains ""
            |> equals false "returns false"
        }

        test "contains abc string null string" {
            "abc"
            |> String.contains null
            |> equals false "returns false"
        }

        test "contains abc string empty string" {
            "abc"
            |> String.contains ""
            |> equals true "returns true"
        }

        test "contains abc string a string" {
            "abc"
            |> String.contains "a"
            |> equals true "returns true"
        }

        test "contains abc string b string" {
            "abc"
            |> String.contains "b"
            |> equals true "returns true"
        }

        test "contains abc string c string" {
            "abc"
            |> String.contains "c"
            |> equals true "returns true"
        }

        test "contains abc string abcd string" {
            "abc"
            |> String.contains "abcd"
            |> equals false "returns false"
        }

        test "equals null string null string" {
            null
            |> String.equals null
            |> equals true "returns true"
        }

        test "equals null string empty string" {
            null
            |> String.equals ""
            |> equals false "returns false"
        }

        test "equals a string A string" {
            "a"
            |> String.equals "A"
            |> equals false "returns false"
        }

        test "equalsCapsInsens a string A string" {
            "a"
            |> String.equalsCapInsens "A"
            |> equals true "returns true"
        }

        test "subString of a null string will yield" {
            null
            |> String.substring 0 1
            |> equals "" "returns an empty string"
        }    

        test "subString of an empty string will yield" {
            ""
            |> String.substring 0 1
            |> equals "" "returns an empty string"
        }    

        test "subString 0 1 of abc string will yield" {
            "abc"
            |> String.substring 0 1
            |> equals "a" "returns a"
        }

        test "subString 1 1 of abc string will yield" {
            "abc"
            |> String.substring 1 1
            |> equals "b" "returns b"
        }

        test "subString 0 0 of abc string will yield" {
            "abc"
            |> String.substring 0 0
            |> equals "" "returns an empty string"
        }

        test "subString 1 -1 of abc string will yield" {
            "abc"
            |> String.substring 1 -1
            |> equals "a" "returns an a"
        }

        test "subString 1 -2 of abc string will yield" {
            "abc"
            |> String.substring 1 -2
            |> equals "" "returns an empty string"
        }

        test "startsWith null string with null string" {
            null
            |> String.startsWith null
            |> equals false "returns false"
        }

        test "startsWith null string with empty string" {
            null
            |> String.startsWith ""
            |> equals false "returns false"
        }

        test "startsWith empty string with null string" {
            ""
            |> String.startsWith null
            |> equals false "returns false"
        }

        test "startsWith abc string with a string" {
            "abc"
            |> String.startsWith "a"
            |> equals true "returns true"
        }

        test "startsWith abc string with abc string" {
            "abc"
            |> String.startsWith "abc"
            |> equals true "returns true"
        }

        test "startsWith abc string with abcd string" {
            "abc"
            |> String.startsWith "abcd"
            |> equals false "returns false"
        }

        test "startsWith abc string with A string" {
            "abc"
            |> String.startsWith "A"
            |> equals false "returns false"
        }

        test "startsWithCapsInsens abc string with A string" {
            "abc"
            |> String.startsWithCapsInsens "A"
            |> equals true "returns true"
        }

        test "restString of null string" {
            null
            |> String.restString
            |> equals "" "returns empty string"
        }

        test "restString of empty string" {
            ""
            |> String.restString
            |> equals "" "returns empty string"
        }

        test "restString of a string" {
            "a"
            |> String.restString
            |> equals "" "returns empty string"
        }

        test "restString of abc string" {
            "abc"
            |> String.restString
            |> equals "bc" "returns bc string"
        }

    ]



