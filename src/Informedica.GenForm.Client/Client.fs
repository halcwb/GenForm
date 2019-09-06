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


module Speech =

    open Fable.Core

    [<Emit("window.speechSynthesis.speak(new SpeechSynthesisUtterance($0));")>]
    let speak s = ()


module Timer =

    open Browser
    
    type Model = 
        {
            Start : DateTime
            Current: DateTime
        }

    type Msg = Tick of DateTime

    let tick dispatch =
        window.setInterval(fun _ ->
            dispatch (Tick DateTime.Now)
        , 1000) |> ignore

    let init () =
        let now = DateTime.Now
        {
            Start = now
            Current = now
        }

    let secondsPast model =
        (model.Current - model.Start).Duration()

    let update (Tick next) model =
        { model with Current = next }
        

// == HELPER FUNCTIONS ==


// === MODEL ===

type Model = 
    { 
        HelloWorld : string
        Test : string option
    }

type Msg =
    | HelloWorld
    | GetTest of string


let init () : Model * Cmd<Msg> =
    { HelloWorld = "Hello World"; Test = None  }, Cmd.none

let fetchTest () =
    promise {
        let! response = Fetch.fetch "/test" []
        return! response.text()
    }

let update (msg: Msg) (model : Model) : Model * Cmd<Msg> =
    match msg with
    | HelloWorld  -> model, Cmd.OfPromise.perform fetchTest () GetTest
    | GetTest s -> { model with Test = Some s }, Cmd.none


// === STYLES ===



// === VIEW FUNCIONS ===


let view (model : Model) (dispatch : Msg -> unit) =

    div [ ]
        [ 
            yield button [ OnClick (fun _ -> HelloWorld |> dispatch) ] [ str "Test"]
            yield typography [] [ str model.HelloWorld ]
            match model.Test with | Some s -> yield typography [] [ str s ] | None -> ()
        ]  


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
