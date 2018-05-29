#load "gencoderefs.fsx"

#time

open System

let pwd = Environment.GetEnvironmentVariable("HOME")
Environment.CurrentDirectory <- pwd + "/Development/GenForm/" 

open Informedica.GenProduct.Lib
open Informedica.GenUtils.Lib

// File
File.exists <| FilePath.GStandPath + "BST000T"

CodeGen.generateZIndex (CodeGen.tabelList)
|> File.writeTextToFile "Zindex.fs"
