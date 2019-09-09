module Client

open System
open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fetch.Types
open Thoth.Fetch
open Thoth.Json
open Fable.MaterialUI.Core
open Fable.MaterialUI.Props
open Fable.MaterialUI.Themes

open Extensions.Fable.MaterialUI.Core

open Views

// == HELPER FUNCTIONS ==


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


type Model = 
    | EmptyModel
    | MainGroups of StringList
    | TherapyGroups of StringList
    | SubGroups of StringList
    | PharmacoGroups of StringList
    | Generics of StringList
and StringList = (string list)

let fetchStringList (key, value) =
    let url = sprintf "/api?%s=%s" key value
    promise {
        let! response = Fetch.fetch url []
        let! grps = response.json<string[]>()
        return (grps |> Array.toList)
    }

let init () : Model * Cmd<Msg> =
    EmptyModel, Cmd.OfPromise.perform fetchStringList ("main", "") MainGroupsFetched


let update (msg: Msg) (model : Model) : Model * Cmd<Msg> =
    let fetch k v m = model, Cmd.OfPromise.perform fetchStringList (k, v) m
    match msg with
    | Reset _ -> init ()
    | MainGroupsFetched gl ->     gl |> MainGroups, Cmd.none
    | TherapyGroupsFetched gl ->  gl |> TherapyGroups, Cmd.none
    | SubGroupsFetched gl ->      gl |> SubGroups, Cmd.none
    | PharmacoGroupsFetched gl -> gl |> PharmacoGroups, Cmd.none
    | GenericsFetched gl ->       gl |> Generics, Cmd.none
    | MainGroupSelect s ->        fetch "tgp" s TherapyGroupsFetched
    | TherapyGroupSelect s ->     fetch "sub" s SubGroupsFetched
    | SubGroupSelect s ->         fetch "phg" s PharmacoGroupsFetched
    | PharmacoGroupSelect s ->    fetch "gen" s GenericsFetched
    | GenericSelect s -> 
        printfn "selected: %s" s
        model, Cmd.none


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
        | EmptyModel -> div [] []

    div [ ]
        [ 
            headerBar dispatch
            content
        ] 
    |> (fun el -> container [ MaxWidth ContainerMaxWidth.Sm ] [el])
    


#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
//|> Program.withSubscription subscription
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
