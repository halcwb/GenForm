#load "gencoderefs.fsx"

#time

open System

open Informedica.GenProduct.Lib
open Informedica.GenUtils.Lib

// File
File.exists <| FilePath.GStandPath + "BST000T"

CodeGen.generateZIndex (CodeGen.tabelList)
|> File.writeTextToFile "Zindex.fs"

Environment.CurrentDirectory
