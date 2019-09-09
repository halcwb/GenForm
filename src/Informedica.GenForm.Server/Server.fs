namespace Informedica.GenForm.Service

module Main =
    
    open System
    open System.IO
    open Microsoft.AspNetCore.Http
    open Giraffe
    open Saturn

    open Informedica.GenUtils.Lib.BCL
    open Informedica.GenForm.Lib

    open HttpsConfig
    open Informedica.GenForm.Lib
    open Informedica.GenProduct.Lib

    module GPP = GenPresProduct
    module ATC = ATCGroup
        
    let getMain     (atc: ATCGroup.ATCGroup) = atc.AnatomicalGroup
    let getTherapy  (atc: ATCGroup.ATCGroup) = atc.TherapeuticMainGroup 
    let getSub      (atc: ATCGroup.ATCGroup) = atc.TherapeuticSubGroup 
    let getPharmaco (atc: ATCGroup.ATCGroup) = atc.PharmacologicalGroup 
    let getGeneric  (atc: ATCGroup.ATCGroup) = 
        sprintf "%s %s" atc.Generic atc.Shape

    let getMainGroups () =
        ATC.get ()
        |> Array.map (fun atc ->
            atc.AnatomicalGroup
        )
        |> Array.distinct
        |> Array.sort

    let getGroups (get1 : ATCGroup.ATCGroup -> string)
                  (get2 : ATCGroup.ATCGroup -> string) 
                  (v : string) =
        ATC.get ()
        |> Array.collect (fun atc ->
            let g = (atc |> get1).ToLower()
            if v.ToLower() = g then
                [| (atc |> get2) |]
            else Array.empty
        )
        |> Array.distinct
        |> Array.sort

    let getTherapyGroups =  getGroups getMain     getTherapy
    let getSubGroups =      getGroups getTherapy  getSub
    let getPharmacoGroups = getGroups getSub      getPharmaco
    let getGenerics =       getGroups getPharmaco getGeneric
        

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
        

    let processRuleRequest rr =
        rr
        |> toDto
        |> (fun dto -> printfn "request: %A" dto.GPK; dto)
        |> Dto.processDto
        |> (fun dto' -> printfn "response: %A" dto'.Label; dto')


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


    let handleApi = 
        fun (next: HttpFunc) (ctx : HttpContext) ->
            [ "main"; "tgp"; "sub"; "phg"; "gen" ]
            |> List.fold (fun acc k ->
                acc
                |> function 
                | Some _, Some _ -> acc
                | _, _ ->
                    k
                    |> ctx.TryGetQueryStringValue
                    |> fun v -> 
                        printfn "request %s: %A" k v
                        (Some k, v)
            ) (None, None)
            |> function 
            | Some k, Some _ when k = "main" -> getMainGroups ()
            | Some k, Some v when k = "tgp"  -> getTherapyGroups v
            | Some k, Some v when k = "sub"  -> getSubGroups v
            | Some k, Some v when k = "phg"  -> getPharmacoGroups v
            | Some k, Some v when k = "gen"  -> getGenerics v
            | _, _ -> Array.empty
            |> fun xs -> 
                printfn "returning %i items" (xs |> Array.length)
                json xs next ctx


    let webApp =
        choose [
            route "/request" >=> handleRequest
            route "/api" >=> handleApi
            route "/html" >=> handleHtml 
        ]


    let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

    let publicPath = Path.GetFullPath "../Client/public"

    let port =
        "SERVER_PORT"
        |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us


    let app = application {
        url ("http://0.0.0.0:" + port.ToString() + "/")
        use_router webApp
        memory_cache
        use_static publicPath
        use_json_serializer(Thoth.Json.Giraffe.ThothSerializer())
        use_gzip
    }

    run app


    //// ---------------------------------
    //// Error handler
    //// ---------------------------------

    //let errorHandler (ex : Exception) (logger : ILogger) =
    //    logger.LogError(EventId(), ex, "An unhandled exception has occurred while executing the request.")
    //    clearResponse >=> setStatusCode 500 >=> text ex.Message

    //// ---------------------------------
    //// Config and Main
    //// ---------------------------------

    //let configureCors (builder : CorsPolicyBuilder) =
    //    builder.WithOrigins("http://localhost:8085")
    //           .AllowAnyMethod()
    //           .AllowAnyHeader()
    //           |> ignore

    //let configureApp (app : IApplicationBuilder) =
    //    let env = app.ApplicationServices.GetService<IHostingEnvironment>()
    //    (match env.IsDevelopment() with
    //    | true  -> app.UseDeveloperExceptionPage()
    //    | false -> app.UseGiraffeErrorHandler errorHandler)
    //        .UseStaticFiles()
    //        .UseCors(configureCors)
    //        .UseGiraffe(webApp)

    //let configureServices (services : IServiceCollection) =
    //    services.AddCors()    |> ignore
    //    services.AddGiraffe() |> ignore

    //let configureLogging (builder : ILoggingBuilder) =
    //    let filter (l : LogLevel) = l.Equals LogLevel.Error
    //    builder.AddFilter(filter).AddConsole().AddDebug() |> ignore

    //[<EntryPoint>]
    //let main _ =
    //    // Load GenForm
    //    let dt = DateTime.now ()
    //    printfn "loading GenForm: %s" (dt.ToString("hh:mm"))
    //    Dto.loadGenForm ()
    //    let time = DateTime.now () - dt
    //    printfn "ready in: %i seconds" (time.Seconds)

    //    let contentRoot = Directory.GetCurrentDirectory()
    //    let webRoot     = Path.Combine(contentRoot, "../Client/public/")

    //    let endpoints =
    //        [ EndpointConfiguration.Default ]

    //    WebHostBuilder()
    //        .UseKestrel(fun o -> o.ConfigureEndpoints endpoints)
    //        .UseContentRoot(contentRoot)
    //        .UseIISIntegration()
    //        .UseWebRoot(webRoot)
    //        .Configure(Action<IApplicationBuilder> configureApp)
    //        .ConfigureServices(configureServices)
    //        .ConfigureLogging(configureLogging)
    //        .Build()
    //        .Run()
    //    0
