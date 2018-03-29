namespace Informedica.GenProduct.Tests

module Main = 

    open Expecto

    [<EntryPoint>]
    let main argv =
        Tests.runTestsInAssembly defaultConfig argv
