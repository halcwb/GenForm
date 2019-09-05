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
    open Informedica.GenForm.Lib
    open Informedica.GenProduct.Lib

        
    type RuleRequest () =
        member val age = 0. with get, set
        member val wth = 0. with get, set
        member val hgt = 0. with get, set
        member val gpk = "" with get, set
        member val gen = "" with get, set
        member val shp = "" with get, set
        member val rte = "" with get, set
        member val unt = "" with get, set
        member val run = "" with get, set
        member val isr = false with get, set


    let toDto (req : RuleRequest) =
        { 
            Dto.dto with
                AgeInMo = req.age
                WeightKg = req.wth 
                LengthCm = req.hgt
                GPK = 
                    match req.gpk |> Int32.tryParse with
                    | Some i -> i
                    | None -> 0
                Generic = req.gen
                Shape = req.shp
                Route = req.rte
                MultipleUnit = req.unt
                RateUnit = req.run
                IsRate = req.isr
        }
        


    let testDto =
        {
            Dto.dto with
                AgeInMo = 12.
                WeightKg = 10.
                GPK = 9504
                Route = "oraal"
        }


    let handleTestRequest =
        fun (next : HttpFunc) (ctx : HttpContext) ->    
            testDto
            |> (fun dto -> printfn "request: %A" dto; dto)
            |> Dto.processDto
            |> (fun dto' -> printfn "response: %A" dto'; dto')
            |> (fun dto' -> 
                    match dto' with 
                    | _ -> json dto' next ctx
                    //| Some r -> json r next ctx
                    //| None   -> json dto next ctx
                )


    let processRuleRequest rr =
        rr
        |> toDto
        |> (fun dto -> printfn "request: %A" dto; dto)
        |> Dto.processDto
        |> (fun dto' -> printfn "response: %A" dto'; dto')


    let handleRequest =
        fun (next : HttpFunc) (ctx : HttpContext) ->    
            ctx.BindQueryString<RuleRequest>()
            |> processRuleRequest
            |> (fun dto' -> json dto' next ctx)


    let handleHtml =
        fun (next : HttpFunc) (ctx : HttpContext) ->    
            ctx.BindQueryString<RuleRequest>()
            |> processRuleRequest
            |> (fun dto' -> Giraffe.ResponseWriters.htmlString dto'.Text next ctx)


    let webApp =
        choose [
            route "/test"    >=> handleTestRequest  // json Dto.testDto
            route "/request" >=> handleRequest
            route "/html" >=> handleHtml ]


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
            .UseStaticFiles()
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
