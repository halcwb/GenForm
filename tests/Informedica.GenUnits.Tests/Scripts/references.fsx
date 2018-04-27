#load @".\..\..\..\.paket\load\netstandard2.0\main.group.fsx"
#load @".\..\..\..\.paket\load\netcoreapp2.0\main.group.fsx"

// Need to update this for different machines
#r @"C:\Program Files\dotnet\sdk\NuGetFallbackFolder\microsoft.netcore.app\2.0.0\ref\netcoreapp2.0\netstandard.dll"

#r @".\..\..\..\src\Informedica.GenUtils.Lib\bin\Debug\netstandard2.0\Informedica.GenUtils.Lib.dll"

#load @"./../../../src/Informedica.GenUnits.Lib/Constants.fs"
#load @"./../../../src/Informedica.GenUnits.Lib/Unit.fs"
#load @"./../../../src/Informedica.GenUnits.Lib/CombiUnit.fs"
#load @"./../../../src/Informedica.GenUnits.Lib/UnitGroup.fs"
#load @"./../../../src/Informedica.GenUnits.Lib/ValueUnit.fs"
#load @"./../../../src/Informedica.GenUnits.Lib/Api.fs"

