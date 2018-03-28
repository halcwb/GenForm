namespace Informedica.GenForm.Service

module Main =
    
    open System
    open System.IO
    open Microsoft.AspNetCore.Http
    open Microsoft.AspNetCore.Builder
    open Microsoft.AspNetCore.Hosting
    open Microsoft.AspNetCore.Authentication
    open Microsoft.AspNetCore.Cors.Infrastructure
    open Microsoft.Extensions.Configuration
    open Microsoft.Extensions.Logging
    open Microsoft.Extensions.DependencyInjection
    open Microsoft.AspNetCore.Server.Kestrel.Core
    open Microsoft.AspNetCore.Server.Kestrel
    open Giraffe

    open Newtonsoft
    open Informedica.GenForm.Lib

    open HttpsConfig

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


    // ---------------------------------
    // Error handler
    // ---------------------------------

    let errorHandler (ex : Exception) (logger : ILogger) =
        logger.LogError(EventId(), ex, "An unhandled exception has occurred while executing the request.")
        clearResponse >=> setStatusCode 500 >=> text ex.Message

    // ---------------------------------
    // Config and Main
    // ---------------------------------

    let configureCors (builder : CorsPolicyBuilder) =
        builder.WithOrigins("http://localhost:8080")
               .AllowAnyMethod()
               .AllowAnyHeader()
               |> ignore

    let configureApp (app : IApplicationBuilder) =
        let env = app.ApplicationServices.GetService<IHostingEnvironment>()
        (match env.IsDevelopment() with
        | true  -> app.UseDeveloperExceptionPage()
        | false -> app.UseGiraffeErrorHandler errorHandler)
            .UseCors(configureCors)
            .UseGiraffe(webApp)

    let configureServices (services : IServiceCollection) =
        services.AddCors()    |> ignore
        services.AddGiraffe() |> ignore

    let configureLogging (builder : ILoggingBuilder) =
        let filter (l : LogLevel) = l.Equals LogLevel.Error
        builder.AddFilter(filter).AddConsole().AddDebug() |> ignore

    [<EntryPoint>]
    let main _ =
        let contentRoot = Directory.GetCurrentDirectory()
        let webRoot     = Path.Combine(contentRoot, "WebRoot")

        let endpoints =
            [ EndpointConfiguration.Default ]

        WebHostBuilder()
            .UseKestrel(fun o -> o.ConfigureEndpoints endpoints)
            .UseContentRoot(contentRoot)
            .UseIISIntegration()
            .UseWebRoot(webRoot)
            .Configure(Action<IApplicationBuilder> configureApp)
            .ConfigureServices(configureServices)
            .ConfigureLogging(configureLogging)
            .Build()
            .Run()
        0