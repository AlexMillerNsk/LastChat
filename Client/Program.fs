module Program

open Browser
open Elmish
open Elmish.React
open Fable.Core.JS
open Fable.React
open Props

type Model = { State: string
               TextBox: string
               Content: string  }

type Msg =
    | SentRequest
    | SetTextBox of string
    | SetResponse of string
    | SetChat of string

let webSocket = WebSocket.Create("ws://127.0.0.1:8080/websocket")
let registerOnMessageHandler =
    fun dispatch ->
        async {
            webSocket.addEventListener_message (fun xy -> dispatch <| SetChat (xy.data.ToString()))
        } |> Async.StartImmediate

let init() = { State = "test"
               TextBox = ""
               Content = "jjhhjj" }, [ registerOnMessageHandler ]
let sendRequest model = fun _ -> async { webSocket.send($"{model.TextBox}") } |> Async.StartImmediate
    
let update msg model =
    match msg with
    | SentRequest    -> model, [ sendRequest model ]
    | SetTextBox v   -> { model with TextBox = v }, Cmd.none
    | SetResponse r  -> { model with State = r }, Cmd.none
    | SetChat x      -> { model with Content = x }, Cmd.none


let view (model: Model) dispatch =
    div [] [
        div [] [ str model.State ]
        button [ OnClick (fun _ -> dispatch SentRequest) ] [ str "SendRequest" ]
        div [  ] [ input [ Value model.TextBox; OnChange (fun e -> dispatch (SetTextBox e.Value) ) ] ]
        str (model.Content.ToString()) ]

Program.mkProgram init update view
//|> Program.withSubscription onMessage 
|> Program.withReactSynchronous "elmish-app"
|> Program.run