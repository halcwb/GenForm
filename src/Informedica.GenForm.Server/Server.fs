namespace Informedica.GenForm.Service


module GStandExt =

    open Informedica.GenForm.Lib

    open GStand
    open Informedica.GenUtils.Lib.BCL

    module Dosage = DoseRule.Dosage
    
    module RF = Informedica.GenProduct.Lib.RuleFinder 
    module DR = Informedica.GenProduct.Lib.DoseRule
    module GPP = Informedica.GenProduct.Lib.GenPresProduct

    let cfg = { UseAll = true ; IsRate = false ; SubstanceUnit = None ; TimeUnit = None }

    let cfgmcg = { cfg with SubstanceUnit = (Some ValueUnit.Units.mcg) }

    let createWithCfg cfg = GStand.createDoseRules cfg None None None None

    let createDoseRules = createWithCfg cfg
    
    let createCont su tu = 
        let cfg = { cfg with IsRate = true ; SubstanceUnit = su ; TimeUnit = tu }
        GStand.createDoseRules cfg None None None None

    let mdText = """
## _Stofnaam_: {generic}
Synoniemen: {synonym}

---

### _ATC code_: {atc}

### _Therapeutische groep_: {thergroup} 

### _Therapeutische subgroep_: {thersub}

### _Generiek groep_: {gengroup}

### _Generiek subgroep_: {gensub}

"""

    let mdIndicationText = """

---

### _Indicatie_: {indication}
"""


    let mdRouteText = """
* _Route_: {route}
"""

    let mdShapeText = """
  * _Vorm_: {shape}
  * _Producten_: 
  * {products}
"""

    let mdPatientText = """
    * _Patient_: __{patient}__
"""

    let mdDosageText = """
      {dosage}

"""


    let mdConfig = 
        {
            DoseRule.mdConfig with
                MainText = mdText
                IndicationText = mdIndicationText
                RouteText = mdRouteText
                ShapeText = mdShapeText
                PatientText = mdPatientText
                DosageText = mdDosageText
        }


    let toStr = DoseRule.toStringWithConfig mdConfig false

    let printDoseRules rs = 
        rs
        |> Seq.map (fun dr -> 
            dr 
            |> toStr
            |> Markdown.toHtml
        )



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

    let atcToGen (atc : ATCGroup.ATCGroup) =
        atc.Routes
        |> String.splitAt ','
        |> Array.map (fun r ->
            sprintf "%s %s %s" atc.Generic (r.Trim()) atc.Shape
        )
        
    let getMain     (atc: ATCGroup.ATCGroup) = atc.AnatomicalGroup      |> Array.singleton
    let getTherapy  (atc: ATCGroup.ATCGroup) = atc.TherapeuticMainGroup |> Array.singleton
    let getSub      (atc: ATCGroup.ATCGroup) = atc.TherapeuticSubGroup  |> Array.singleton
    let getPharmaco (atc: ATCGroup.ATCGroup) = atc.PharmacologicalGroup |> Array.singleton
    let getGeneric  (atc: ATCGroup.ATCGroup) = atc |> atcToGen

    let getMainGroups () =
        ATC.get ()
        |> Array.map (fun atc ->
            atc.AnatomicalGroup
        )
        |> Array.distinct
        |> Array.sort

    let getGroups (get1 : ATCGroup.ATCGroup -> string[])
                  (get2 : ATCGroup.ATCGroup -> string[]) 
                  (v : string) =
        ATC.get ()
        |> Array.collect (fun atc ->
            let g = 
                match atc |> get1 with
                | [|g|] -> g.ToLower()
                | _ -> ""
            if v.ToLower() = g then
                atc |> get2
            else Array.empty
        )
        |> Array.distinct
        |> Array.sort

    let getTherapyGroups =  getGroups getMain     getTherapy
    let getSubGroups =      getGroups getTherapy  getSub
    let getPharmacoGroups = getGroups getSub      getPharmaco
    let getGenerics =       getGroups getPharmaco getGeneric
        
    let getRules v =
        ATC.get ()
        |> Array.tryFind (fun atc ->
            atc
            |> atcToGen
            |> Array.exists ((=) v)
        )
        |> function 
        | None -> Array.empty
        | Some atc -> 
            let r =
                atc.Routes
                |> String.splitAt ','
                |> Array.find (fun s -> v |> String.contains s)
            GStandExt.createDoseRules atc.Generic atc.Shape r
            |> GStandExt.printDoseRules
            |> Seq.toArray


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
            [ "main"; "tgp"; "sub"; "phg"; "gen"; "rul" ]
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
            | Some k, Some v when k = "rul"  -> getRules v
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

    do
        Dto.loadGenForm ()
        run app


