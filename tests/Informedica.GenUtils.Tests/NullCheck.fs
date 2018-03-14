module NullCheckTests

open Expecto

open Informedica.GenUtils.Lib


[<Tests>]
let tests =

    testList "NullCheck returns " [

        test "true when checking " {
            Expect.equal (NullCheck.isNull null) true " null "
        }
      
        test "false when checking " {
            Expect.equal (NullCheck.isNull "blah") false " a string "
        }
  
    ]