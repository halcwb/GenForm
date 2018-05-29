#load @".\..\..\..\.paket\load\netstandard2.0\main.group.fsx"

// Need to update this for different machines
#r @"C:\Program Files\dotnet\sdk\NuGetFallbackFolder\microsoft.netcore.app\2.0.0\ref\netcoreapp2.0\netstandard.dll"

#r @".\..\..\..\src\Informedica.GenUtils.Lib\bin\Debug\netstandard2.0\Informedica.GenUtils.Lib.dll"
#r @".\..\..\..\src\Informedica.GenUnits.Lib\bin\Debug\netstandard2.0\Informedica.GenUnits.Lib.dll"

#load @"./../../../src/Informedica.GenProduct.Lib/FilePath.fs"
#load @"./../../../src/Informedica.GenProduct.Lib/Json.fs"
#load @"./../../../src/Informedica.GenProduct.Lib/Parser.fs"
#load @"./../../../src/Informedica.GenProduct.Lib/BST001T.fs"
#load @"./../../../src/Informedica.GenProduct.Lib/BST000T.fs"
#load @"./../../../src/Informedica.GenProduct.Lib/CodeGen.fs"
