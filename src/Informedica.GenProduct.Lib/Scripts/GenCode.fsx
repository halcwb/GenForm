#load "gencoderefs.fsx"

#time

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ + "/../"

open Informedica.GenProduct.Lib
open Informedica.GenUtils.Lib

CodeGen.generateZIndex (CodeGen.tabelList)
|> File.writeTextToFile "Zindex.fs"
