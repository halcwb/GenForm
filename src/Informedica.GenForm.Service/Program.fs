namespace Informedica.GenForm.Service

module Main =
    
    open System
    open System.IO
    open Microsoft.AspNetCore.Builder
    open Microsoft.AspNetCore.Hosting
    open Microsoft.AspNetCore.Http
    open Microsoft.AspNetCore.Cors.Infrastructure
    open Microsoft.Extensions.Logging
    open Microsoft.Extensions.DependencyInjection
    open Giraffe

    open Newtonsoft
    open Informedica.GenUtils.Lib.BCL
    open Informedica.GenForm.Lib

    open HttpsConfig
        
    type RuleRequest () =
        member val bty = 0 with get, set
        member val btm = 0 with get, set
        member val btd = 0 with get, set
        member val wth = 0. with get, set
        member val hgt = 0. with get, set
        member val gpk = "" with get, set
        member val rte = "" with get, set
        member val unt = "" with get, set


    let handleRequest =
        fun (next : HttpFunc) (ctx : HttpContext) ->    
            let dto = 
                let req = ctx.BindQueryString<RuleRequest>()
                { Dto.dto with
                    BirthYear = req.bty
                    BirthMonth = req.btm
                    BirthDay = req.btd
                    WeightKg = req.wth
                    LengthCm = req.hgt
                    GPK = req.gpk
                    Route = req.rte
                    MultipleUnit = req.unt
                }
            dto
            |> (fun dto -> printfn "request: %A" dto; dto)
            |> Dto.findRules
            |> (fun dto' -> printfn "response: %A" dto'; dto')
            |> (fun dto' -> 
                    match dto' with 
                    | Some r -> json r next ctx
                    | None   -> json dto next ctx
                )

    let webApp =
        choose [
            route "/test" >=> json Dto.testDto
            route "/request" >=> handleRequest ]


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
        // Load GenForm
        let dt = DateTime.now ()
        printfn "loading GenForm: %s" (dt.ToString("hh:mm"))
        Dto.loadGenForm ()
        let time = DateTime.now () - dt
        printfn "ready in: %i seconds" (time.Seconds)

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
