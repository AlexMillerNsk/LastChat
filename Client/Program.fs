module Program

open Browser
open Elmish
open Elmish.React
open Fable.Core.JS
open Fable.React
open Props
open System.Net.Sockets


type Page =
    |Autorisation
    |Chat

type Model = { State: string
               TextBox: string
               Name: string
               Content: string list
               CurrentPage: Page}

type Msg =
    | SentRequest
    | SetTextBox of string
    | SetName of string
    | SetResponse of string
    | SetChat of string
    | Autorisation of Page



let webSocket = WebSocket.Create($"ws://192.168.68.104:8080/websocket")
let registerOnMessageHandler =
    fun dispatch ->
        async {
            webSocket.addEventListener_message (fun xy -> dispatch <| SetChat (xy.data.ToString()))
        } |> Async.StartImmediate

let init() = { State = "test1"
               TextBox = ""
               Name =""
               Content = []
               CurrentPage = Page.Autorisation}, [ registerOnMessageHandler ]
let sendRequest model = fun _ -> async { webSocket.send($"{model.Name}: {model.TextBox}\n") } |> Async.StartImmediate
    
let update msg model =
    match msg with
    | SentRequest    -> model, [ sendRequest model ]
    | SetTextBox v   -> { model with TextBox = v }, Cmd.none
    | SetResponse r  -> { model with State = r }, Cmd.none
    | SetChat x      -> { model with Content = x::model.Content}, Cmd.none
    | Autorisation page -> {model with CurrentPage = page}, Cmd.none
    | SetName x      -> {model with Name = x}, Cmd.none


let view (model: Model) dispatch =
    match model.CurrentPage with
    | Page.Chat ->
        div [] [
            div [] [ str model.State ]
            button [ OnClick (fun _ -> dispatch SentRequest) ] [ str "SendRequest" ]
            div [  ] [ input [ Value model.TextBox; OnChange (fun e -> dispatch (SetTextBox e.Value) ) ] ]
            str (model.Content.ToString()) ]
    | Page.Autorisation ->
            div [] [
            button [ OnClick (fun _ -> dispatch (Autorisation Page.Chat)) ] [ str "autorise" ]
            div [  ] [ input [ Value model.Name; OnChange (fun e -> dispatch (SetName e.Value) ) ] ]
            str (model.Name.ToString()) ]


Program.mkProgram init update view
//|> Program.withSubscription onMessage 
|> Program.withReactSynchronous "elmish-app"
|> Program.run