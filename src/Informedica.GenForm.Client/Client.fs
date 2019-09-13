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

    let toHash = function
        | Page.MainGroups -> "#main"
        | Page.TherapyGroups s -> "#thg" + "/" + s
        | Page.SubGroups s -> "#sub" + "/" + s
        | Page.PharmacoGroups s -> "#phg" + "/" + s
        | Page.Generics s -> "#gen" + "/" + s
        | Page.Rules s -> "#rul" + "/" + s

    let pageParser =
        oneOf [
            map Page.MainGroups (s "main")
            map Page.TherapyGroups (s "thg" </> str)
            map Page.SubGroups (s "sub" </> str)
            map Page.PharmacoGroups (s "phg" </> str)
            map Page.Generics (s "gen" </> str)
            map Page.Rules (s "rul" </> str)
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
        | None -> model, Cmd.none


let init _ : Model * Cmd<Msg> =
    Loading, Cmd.OfPromise.perform fetchStringList ("main", "") MainGroupsFetched


let update (msg: Msg) (model : Model) : Model * Cmd<Msg> =
    let nav p= 
        model,
        Navigation.toHash p
        |> Elmish.Navigation.Navigation.newUrl

    match msg with
    | Reset _ -> init ()
    | MainGroupsFetched gl ->     gl |> MainGroups, Cmd.none
    | TherapyGroupsFetched gl ->  gl |> TherapyGroups, Cmd.none
    | SubGroupsFetched gl ->      gl |> SubGroups, Cmd.none
    | PharmacoGroupsFetched gl -> gl |> PharmacoGroups, Cmd.none
    | GenericsFetched gl ->       gl |> Generics, Cmd.none
    | RulesFetched rl ->          rl |> Rules, Cmd.none
    | MainGroupSelect s ->        s |> Navigation.Page.TherapyGroups |> nav
    | TherapyGroupSelect s ->     s |> Navigation.Page.SubGroups |> nav
    | SubGroupSelect s ->         s |> Navigation.Page.PharmacoGroups |> nav
    | PharmacoGroupSelect s ->    s |> Navigation.Page.Generics |> nav
    | GenericSelect s ->          s |> Navigation.Page.Rules |> nav


// === STYLES ===



// === VIEW FUNCIONS ===

let filler = 
    div [ Style [ (FlexGrow 1) ] ] [ ]

let headerBar dispatch =
    Views.NavBar.view "GenForm" dispatch

let createList dispatch xs =
    let typo s =
        typography [ TypographyProp.Align TypographyAlign.Left
                     TypographyProp.Variant TypographyVariant.Button ] [ str s ]
    xs 
    |> List.map (fun x ->
        button [ OnClick (fun _ -> x |> dispatch) ] [ typo x ]
        |> List.singleton
        |> listItem [ Style [ (FlexGrow 1) ]]
    )
    |> list [ Style [ CSSProp.MarginTop "80px" ] ]
 
let view (model : Model) (dispatch : Msg -> unit) =
    let content =
        match model with
        | MainGroups xs ->     xs |> createList (MainGroupSelect >> dispatch)
        | TherapyGroups xs ->  xs |> createList (TherapyGroupSelect >> dispatch)
        | SubGroups xs ->      xs |> createList (SubGroupSelect >> dispatch)
        | PharmacoGroups xs -> xs |> createList (PharmacoGroupSelect >> dispatch)
        | Generics xs ->       xs |> createList (GenericSelect >> dispatch)
        | Rules xs ->
            xs
            |> List.map innerHtml
            |> div [ Style [ CSSProp.MarginTop "80px"; CSSProp.MarginBottom "20px" ] ]
        | Loading -> typography [ Style [ CSSProp.MarginTop "80px"; CSSProp.MarginBottom "20px" ]
                                  TypographyProp.Variant TypographyVariant.H2 ] 
                                [ str "Loading..." ]

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
