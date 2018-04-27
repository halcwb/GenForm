#load @".\..\..\..\.paket\load\netstandard2.0\main.group.fsx"

// Need to update this for different machines
#r @"C:\Program Files\dotnet\sdk\NuGetFallbackFolder\microsoft.netcore.app\2.0.0\ref\netcoreapp2.0\netstandard.dll"

#load @"./../../../src/Informedica.GenUtils.Lib/Continuation.fs"
#load @"./../../../src/Informedica.GenUtils.Lib/Memoization.fs"
#load @"./../../../src/Informedica.GenUtils.Lib/Reflection.fs"
#load @"./../../../src/Informedica.GenUtils.Lib/NullCheck.fs"
#load @"./../../../src/Informedica.GenUtils.Lib/BCL/Char.fs"
#load @"./../../../src/Informedica.GenUtils.Lib/BCL/String.fs"
#load @"./../../../src/Informedica.GenUtils.Lib/BCL/Int32.fs"
#load @"./../../../src/Informedica.GenUtils.Lib/BCL/Double.fs"
#load @"./../../../src/Informedica.GenUtils.Lib/BCL/BigRational.fs"
#load @"./../../../src/Informedica.GenUtils.Lib/BCL/DateTime.fs"
#load @"./../../../src/Informedica.GenUtils.Lib/Option.fs"
#load @"./../../../src/Informedica.GenUtils.Lib/Array.fs"
#load @"./../../../src/Informedica.GenUtils.Lib/List.fs"
#load @"./../../../src/Informedica.GenUtils.Lib/Seq.fs"
#load @"./../../../src/Informedica.GenUtils.Lib/File.fs"
#load @"./../../../src/Informedica.GenUtils.Lib/App.fs"

