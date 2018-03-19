namespace Informedica.GenProduct.Lib

module Json =

    open System.IO
    open Newtonsoft.Json

    open Informedica.GenUtils.Lib

    ///
    let serialize x =
        JsonConvert.SerializeObject(x)


    let deSerialize<'T> (s: string) =
        JsonConvert.DeserializeObject<'T>(s)

    let cache p o =
        o
        |> serialize
        |> File.writeTextToFile p

    let clearCache c =
        File.Delete(File.groupCache)
        File.Delete(File.substanceCache)
        File.Delete(File.productCache)
        File.Delete(File.ruleCache)

    let getCache<'T> p =
        printfn "Reading cache: %s" p
        File.readAllLines p
        |> String.concat ""
        |> deSerialize<'T>