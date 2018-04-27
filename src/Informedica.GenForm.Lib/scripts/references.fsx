#load @".\..\..\..\.paket\load\netstandard2.0\main.group.fsx"

// Need to update this for different machines
#r @"C:\Program Files\dotnet\sdk\NuGetFallbackFolder\microsoft.netcore.app\2.0.0\ref\netcoreapp2.0\netstandard.dll"


#r @".\..\..\..\src\Informedica.GenUtils.Lib\bin\Debug\netstandard2.0\Informedica.GenUtils.Lib.dll"
#r @".\..\..\..\src\Informedica.GenUnits.Lib\bin\Debug\netstandard2.0\Informedica.GenUnits.Lib.dll"
#r @".\..\..\..\src\Informedica.GenProduct.Lib\bin\Debug\netstandard2.0\Informedica.GenProduct.Lib.dll"

#load @".\..\Mapping.fs"
#load @".\..\MinMax.fs"
#load @".\..\ValueUnit.fs"
#load @".\..\Route.fs"
#load @".\..\Product.fs"
#load @".\..\Patient.fs"
#load @".\..\DoseRule.fs"
#load @".\..\Dto.fs"

