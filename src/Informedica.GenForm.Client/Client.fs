module Client

open System
open Fable.React
open Fable.React.Props

open Elmish
open Elmish.React


open Fable.MaterialUI.Core
open Fable.MaterialUI.Props
open Fable.MaterialUI.Themes

open Extensions.Fable.MaterialUI.Core

open Views


// == HELPER FUNCTIONS ==

module Debounce =
    open Fable.Core

    [<Import("default", "debounce"); Emit("$0($1, $2, $3)")>]
    let debounce<'f> (_f: 'f) (_interval: int) (_immediate: bool)  : 'f = jsNative


type DangerousInnerHtml =
    { __html : string }


let innerHtml s = 
    div [ DangerouslySetInnerHTML { __html = s } ] []

// === MODEL ===

type Msg =
    | Reset of NavBar.Msg
    | MainGroupsFetched of string list
    | MainGroupSelect of string
    | TherapyGroupsFetched of string list
    | TherapyGroupSelect of string
    | SubGroupsFetched of string list
    | SubGroupSelect of string
    | PharmacoGroupsFetched of string list
    | PharmacoGroupSelect of string
    | GenericsFetched of string list
    | GenericSelect of string
    | RulesFetched of string list
    | Search of string


type Model = 
    | Loading
    | MainGroups of string list
    | TherapyGroups of string list
    | SubGroups of string list
    | PharmacoGroups of string list
    | Generics of string list
    | Rules of string list


let fetchStringList (key, value) =
    let url = sprintf "/api?%s=%s" key value
    promise {
        let! response = Fetch.fetch url []
        let! grps = response.json<string[]>()
        return (grps |> Array.toList)
    }


module Navigation =

    open Elmish.UrlParser

    [<RequireQualifiedAccess>]
    type Page =
        | MainGroups
        | TherapyGroups of string
        | SubGroups of string
        | PharmacoGroups of string
        | Generics of string
        | Rules of string
        | Search of string

    let toHash p = 
        match p with
        | Page.MainGroups -> "main", ""
        | Page.TherapyGroups s -> "thg", s
        | Page.SubGroups s -> "sub", s
        | Page.PharmacoGroups s -> "phg", s
        | Page.Generics s -> "gen", s
        | Page.Rules s -> "rul", s
        | Page.Search s -> "qry", s
        |> fun (s1, s2) ->
            if s2 = "" then sprintf "#%s" s1
            else 
                let s2 = s2.Replace("/", "%2F")
                sprintf "#%s/%s" s1 s2

    let pageParser =
        oneOf [
            map Page.MainGroups (s "main")
            map Page.TherapyGroups (s "thg" </> str)
            map Page.SubGroups (s "sub" </> str)
            map Page.PharmacoGroups (s "phg" </> str)
            map Page.Generics (s "gen" </> str)
            map Page.Rules (s "rul" </> str)
            map Page.Search (s "qry" </> str)
        ]

    let urlParser location = 
        printfn "parsing location: %A" location 
        parseHash pageParser location

    let urlUpdate (result: Page Option) model =
        printfn "url changed to: %A" result
        let fetch k v m =
            printfn "going to fetch %s %s" k v
            Loading, Cmd.OfPromise.perform fetchStringList (k, v) m

        match result with
        | Some Page.MainGroups -> fetch "main" "" MainGroupsFetched
        | Some (Page.TherapyGroups s) -> fetch "tgp" s TherapyGroupsFetched
        | Some (Page.SubGroups s) -> fetch "sub" s SubGroupsFetched
        | Some (Page.PharmacoGroups s)  -> fetch "phg" s PharmacoGroupsFetched
        | Some (Page.Generics s) -> fetch "gen" s GenericsFetched
        | Some (Page.Rules s) -> fetch "rul" s RulesFetched
        | Some (Page.Search s) -> fetch "qry" s GenericsFetched
        | None -> model, Cmd.none


let init _ : Model * Cmd<Msg> =
    Loading, Cmd.OfPromise.perform fetchStringList ("main", "") MainGroupsFetched


let update (msg: Msg) (model : Model) : Model * Cmd<Msg> =
    let nav p= 
        model,
        Navigation.toHash p
        |> Elmish.Navigation.Navigation.newUrl

    let fetched c s gl =
        match gl with
        | [g] -> model, g |> s |> Cmd.ofMsg
        | _   -> gl |> c, Cmd.none
        

    match msg with
    | Reset _ -> init ()
    | MainGroupsFetched gl ->     gl |> fetched MainGroups MainGroupSelect
    | TherapyGroupsFetched gl ->  gl |> fetched TherapyGroups TherapyGroupSelect
    | SubGroupsFetched gl ->      gl |> fetched SubGroups SubGroupSelect
    | PharmacoGroupsFetched gl -> gl |> fetched  PharmacoGroups PharmacoGroupSelect
    | GenericsFetched gl ->       gl |> fetched Generics GenericSelect
    | RulesFetched rl ->          rl |> Rules, Cmd.none
    | MainGroupSelect s ->        s |> Navigation.Page.TherapyGroups |> nav
    | TherapyGroupSelect s ->     s |> Navigation.Page.SubGroups |> nav
    | SubGroupSelect s ->         s |> Navigation.Page.PharmacoGroups |> nav
    | PharmacoGroupSelect s ->    s |> Navigation.Page.Generics |> nav
    | GenericSelect s ->          s |> Navigation.Page.Rules |> nav
    | Search s ->
        printfn "search: %s" s
        match s with
        | _ when s |> String.IsNullOrEmpty -> init ()
        | _ when s |> String.length < 3 -> model, Cmd.none
        | _  ->
            s 
            |> Navigation.Page.Search 
            |> nav


// === STYLES ===



// === VIEW FUNCIONS ===

let filler = 
    div [ Style [ (FlexGrow 1) ] ] [ ]

let headerBar dispatch =
    Views.NavBar.view "GenForm" dispatch


let searchInput dispatch =
    let lbl = typography [] [ str "Zoeken..." ]
    let onInput = 
        let f = fun s ->
            s 
            |> Search
            |> dispatch
        let f = Debounce.debounce f 500 false
        OnInput (fun e -> e.Value |> f)

    textField [ onInput 
                MaterialProp.Label lbl
                HTMLAttr.Type "search"
                Style [ Display DisplayOptions.Flex ] ] [ ]
 

let createList dispatch xs =
    let typo s =
        typography [ TypographyProp.Align TypographyAlign.Left
                     TypographyProp.Variant TypographyVariant.Button ] [ str s ]
    xs 
    |> List.map (fun x ->
        button [ OnClick (fun _ -> x |> dispatch) ] [ typo x ]
        |> List.singleton
        |> listItem [ ]
    )
    |> list []
 
let view (model : Model) (dispatch : Msg -> unit) =
    let content =
        match model with
        | MainGroups xs ->     xs |> createList (MainGroupSelect >> dispatch)
        | TherapyGroups xs ->  xs |> createList (TherapyGroupSelect >> dispatch)
        | SubGroups xs ->      xs |> createList (SubGroupSelect >> dispatch)
        | PharmacoGroups xs -> xs |> createList (PharmacoGroupSelect >> dispatch)
        | Generics xs ->       xs |> createList (GenericSelect >> dispatch)
        | Rules xs ->
            match xs with
            | [] -> [ "# Geen doseer regels gevonden" ]
            | _ ->
                xs
            |> List.map innerHtml
            |> div [ Style [ CSSProp.MarginTop "20px" ] ]
        | Loading -> typography [ TypographyProp.Variant TypographyVariant.H2 ] 
                                [ str "Loading..." ]
        |> fun el -> 
            div [ Style [ CSSProp.MarginTop "100px"
                          CSSProp.MarginBottom "20px" ] 
                ]
                [ searchInput dispatch; el ]

    div [ ]
        [ 
            headerBar dispatch
            content
        ] 
    |> (fun el -> container [ MaxWidth ContainerMaxWidth.Sm ] [el])
    


#if DEBUG
open Elmish.Debug
//open Elmish.HMR
#endif

Program.mkProgram init update view
//|> Program.withSubscription subscription
|> Elmish.Navigation.Program.toNavigable Navigation.urlParser Navigation.urlUpdate
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
