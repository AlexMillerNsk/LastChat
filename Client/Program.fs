module Program

open Browser
open Elmish
open Elmish.React
open Feliz

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



let webSocket = WebSocket.Create($"ws://192.168.0.170:8080/websocket")
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
    | SetChat x      -> { model with
                            TextBox = ""
                            Content = x::model.Content}, Cmd.none
    | Autorisation page -> {model with CurrentPage = page}, Cmd.none
    | SetName x      -> {model with Name = x}, Cmd.none

let appTitle =
  Html.p [
    prop.className "title"
    prop.text "Welcome to our Awesome Chat!"
  ]

let chatList (model: Model) (dispatch: Msg -> unit) =
  Html.ul [
    prop.children [
      for chat in model.Content ->
        Html.li [
          prop.classes ["box"; "subtitle"]
          prop.text chat
        ]
    ]
  ]

let inputField (model: Model) (dispatch: Msg -> unit) =
  Html.div [
    prop.classes [ "field"; "has-addons" ]
    prop.children [
      Html.div [
        prop.classes [ "control"; "is-expanded"]
        prop.children [
          Html.input [
            prop.classes [ "input"; "is-medium" ]
            prop.valueOrDefault model.TextBox
            prop.onChange (SetTextBox >> dispatch)
          ]
        ]
      ]
      Html.div [
        prop.className "control"
        prop.children [
          Html.button [
            prop.classes [ "button"; "is-primary"; "is-medium" ]
            prop.onClick (fun _ -> dispatch SentRequest)
            prop.children [
              Html.i [ prop.classes [ "fa"; "fa-plus" ] ]
            ]
          ]
        ]
      ]
    ]
  ]
let view (model: Model) dispatch =
    match model.CurrentPage with
    | Page.Chat ->
        //div [] [
        //    div [] [ str model.State ]
        //    button [ OnClick (fun _ -> dispatch SentRequest) ] [ str "SendRequest" ]
        //    div [  ] [ input [ Value model.TextBox; OnChange (fun e -> dispatch (SetTextBox e.Value) ) ] ]
        //    str (model.Content.ToString())]
        Html.div [
          prop.style [style.padding 20]
          prop.children [
            appTitle
            inputField model dispatch         
            chatList model dispatch          
          ]     
        ]

            
    | Page.Autorisation ->            
            Html.div [
              prop.style [style.padding 20]
              prop.children [
                  appTitle
                  Html.button [
                    prop.onClick  (fun _ -> dispatch (Autorisation Page.Chat)) 
                    prop.text "Autorise"
                    ]
                  Html.input [ prop.onChange (SetName >> dispatch) ]
                  Html.span $"Your name will be {model.Name}"
               ]
            ]
Program.mkProgram init update view
//|> Program.withSubscription onMessage 
|> Program.withReactSynchronous "elmish-app"
|> Program.run