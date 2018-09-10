# GenFormService
This solution is intended as a microservice running in a Docker container to provide a service to get medication information. 

## Setting up the dotnetcore solution for F# and Docker

To set up the project you need to use the dotnet cli. Do not rely on *Visual Studio* or *Visual Studion Code*. Both will mess up your project files.

1. Use `dotnet new sln -n GenFormService` to create the solution file. This will create a solution file with the name `GenFormService.sln`.
2. Add a new project using `dotnet new classlib -lang F# -o src/Informedica.GenUtils.Lib -n Informedica.GenUtils.Lib`. This will add a project in the `src/Informedica.GenUtils.Lib` folder with the project file: `Informedica.GenUtils.Lib.fsproj`.
3. Add the project to the sln file: `dotnet sln add src/Informedica.GenUtils.Lib/Informedica.GenUtils.Lib.fsproj`. The project is now part of the solution.
4. Go to the project folder and add a package reference to the project using `dotnet add package MathNet.Numerics.FSharp --version 4.4.0`. This will add for example the MathNet.Numerics.FSharp package as a reference. Check for each package if there is a dotnet version available, otherwise you're in trouble.
5. Open the solution folder using *Visual Studion Code* to create a `fsx` file with references for the FSI. Look with `SHFT-CMD-P` for `>FSI: Generate script file with references from project`. You need to open a source file for this command to popup up. You can then with `Save As..` save the file in the appropriate folder. This file can then be used in the FSI to load all required references.
6. **DO NOT USE** *Visual Studio* to add the folder with the `references.fsx` file to the project. This will mess up your project file by adding the `fsx` file as a `<Compile Include="scripts\references.fsx" />` instead of a `<None Include="scripts\references.fsx" />`. So a `None` include instead of a `Compile` include.
7. To add an existing source file, copy the file to your project folder and add the file in the `fsproj` manually to be on the save side.
8. Add the Expecto test template: `dotnet new -i "Expecto.Template::*"`.
9. Add a test project by `dotnet new expecto -o tests/Informedica.GenUtils.Tests -lang F# -n Informedica.GenUtils.Tests`. Add this test project to the solution by `dotnet sln add tests/Informedica.GenUtils.Tests/Informedica.GenUtils.Tests.fsproj`. And add a project reference to the expecto test project: `dotnet add tests/Informedica.GenUtils.Tests/Informedica.GenUtils.Tests.fsproj reference src/Informedica.GenUtils.Lib/Informedica.GenUtils.Lib.fsproj`.

This results in a problem with FSharp.Core package downgrade.

So with paket
1. First create the paket dir, `mkdir .paket`.
2. Download paket.bootstrapper.exe and put it in the paket dir as `paket.exe`.
3. Initialize paket with `mono .paket/paket.exe init`. So, this needs the mono framework :-(.
4. Add `paket.references` files to all projects in the solution
5. Install packages by `mono .paket/paket.exe add <package>`. 
6. Using Expecto you need to restrict Argu to `~> 4`.

Visual Studio it is most convenient to use the FSI with this file. Use of the FSI in *Visual Studion Code* is buggy.

## Developing with `dotnet watch run`. 
Make sure that the data directory is in the root of the main project, i.e. `src/Informedica.GenForm.Service/`. Go to that directory and start up with `dotnet watch run`.

In the browser 2 requests can be run:

1. `localhost:8080/test` Giving a test request and
2. `localhost:8080/request?bty=2017&btm=1&btd=1&wth=10&hgt=100&pgk=3689`