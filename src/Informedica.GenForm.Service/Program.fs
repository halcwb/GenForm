namespace Informedica.GenForm.Service

module Main =
    
    open System
    open Microsoft.AspNetCore.Builder
    open Microsoft.AspNetCore.Hosting
    open Microsoft.Extensions.DependencyInjection
    open Giraffe

    open Newtonsoft
    open Informedica.GenForm.Lib

    let response =
        let test = 
            { Dto.dto with
                BirthYear = 2017
                BirthMonth = 3
                BirthDay = 2
                WeightKg = 10.
                LengthCm = 70.
                GPK = "100331"
                MultipleUnit = "mg"
                Route = "or"
            }

        Dto.findRules test
        |> Option.get
        |> sprintf "%A"
        

    let webApp =
        choose [
            route "/" >=> text response ]

    let configureApp (app : IApplicationBuilder) =
        // Add Giraffe to the ASP.NET Core pipeline
        app.UseGiraffe webApp

    let configureServices (services : IServiceCollection) =
        // Add Giraffe dependencies
        services.AddGiraffe() |> ignore

    [<EntryPoint>]
    let main _ =
        WebHostBuilder()
            .UseKestrel()
            .Configure(Action<IApplicationBuilder> configureApp)
            .ConfigureServices(configureServices)
            .Build()
            .Run()
        0