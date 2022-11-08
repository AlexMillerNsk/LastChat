module Program

open Browser
open Elmish
open Elmish.React
open Feliz
open Shared
open Fable.SimpleJson
open FSharp

type Page =
    |Autorisation
    |Chat


type Model = { State: string
               TextBox: string
               Name: string
               Content: string list
               CurrentPage: Page
               Subscribers: string  }


type Msg =
    | SentMsg
    | SetTextBox of string
    | SetName of string
    | SetResponse of string
    | SetChat of string
    | AutorisationSub of Page
    | SetSubscribers of string


let webSocket = WebSocket.Create($"ws://192.168.0.170:8080/websocket")

let chatDecoder x = 
    let deserializedText = Json.parseAs<WsMessage> x
    match deserializedText.MsgType with
    | SendMessage -> SetChat (deserializedText.Message )
    | AutorisationType OpenAutorisation -> SetSubscribers (deserializedText.Message)
    | AutorisationType ClosedAutorisation -> SetSubscribers (deserializedText.Message)

let registerOnMessageHandler =
    fun dispatch ->
        async {
            webSocket.addEventListener_message (fun xy -> dispatch <| chatDecoder (xy.data.ToString()))
        } |> Async.StartImmediate

let init() = { State = ""
               TextBox = ""
               Name = ""
               Content = []
               CurrentPage = Page.Autorisation
               Subscribers =""}, [ registerOnMessageHandler ]   

let sendRequest model = 
    let msgType = { MsgType = SendMessage ; Message = $"{model.Name}: {model.TextBox}\n"}
    let msgTypeJson = Json.serialize msgType
    fun _ -> async { webSocket.send msgTypeJson } |> Async.StartImmediate

let sendName model = 
    let msgType = { MsgType = AutorisationType OpenAutorisation; Message = $"{model.Name}"}
    let msgTypeJson = Json.serialize msgType   
    fun _ -> async { webSocket.send msgTypeJson}|> Async.StartImmediate
    
let update msg model =
    match msg with
    | SentMsg       ->   model, [ sendRequest model ]
    | SetTextBox v        -> { model with TextBox = v }, Cmd.none
    | SetResponse r       -> { model with State = r }, Cmd.none
    | SetChat x           -> { model with
                                Content = x::model.Content
                                TextBox = ""
                                }, Cmd.none
    | AutorisationSub page -> {model with CurrentPage = page}, [sendName model]
    | SetName x           -> {model with Name = x}, Cmd.none
    | SetSubscribers x    -> {model with Subscribers = x}, Cmd.none


let appTitle =
  Html.p [
    prop.className "title"
    prop.text "Welcome to our Awesome Chat!"
  ]

let div (classes: string list) (children: ReactElement list) =
    Html.div [
        prop.classes classes
        prop.children children
    ]
let renderLists (model: Model) (dispatch: Msg -> unit) =
  div [ "box" ] [
    div [ "columns"; "is-mobile"; "is-vcentered" ] [
      div [ "column" ] [
          Html.ul [
            prop.children [
              let reversed = model.Content|> List.rev
              for chat in reversed ->
                Html.li [
                  prop.classes ["box"; "subtitle"]
                  prop.text chat
                ]
            ]
          ]
        ]
      div [ "column"; "is-narrow" ] [
        div [ "buttons" ] [
            Html.ul [
                prop.classes ["box"; "subtitle"]
                prop.text model.Subscribers
            ]
          ]
        ]
      ]
    ]
  

let chatList (model: Model) (dispatch: Msg -> unit) =
  Html.ul [
    prop.children [
      let reversed = model.Content|> List.rev
      for chat in reversed ->
        Html.li [
          prop.classes ["box"; "subtitle"]
          prop.text chat
        ]
    ]
  ]

let inputField (model: Model) (dispatch: Msg -> unit) =
  div [ "field"; "has-addons" ] [
      div [ "control"; "is-expanded"] [
          Html.input [
            prop.classes [ "input"; "is-medium" ]
            prop.valueOrDefault model.TextBox
            prop.onChange (SetTextBox >> dispatch)
          ]
        
      ]
      div ["control"] [
          Html.button [
            prop.classes [ "button"; "is-primary"; "is-medium" ]
            prop.onClick (fun _ -> dispatch SentMsg)
            prop.children [
              Html.i [ prop.classes [ "fa"; "fa-plus" ] ]
            ]
          ]
      ]
        
  ] 
      
    

let view (model: Model) dispatch =
    match model.CurrentPage with
    | Page.Chat ->
        Html.div [
          prop.style [style.padding 20]
          prop.children [
            appTitle
            inputField model dispatch         
            renderLists model dispatch          
          ]     
        ]           
    | Page.Autorisation ->            
            Html.div [
              prop.style [style.padding 20]
              prop.children [
                  appTitle
                  Html.button [
                    prop.classes [ "button"; "is-primary"; "is-medium" ]
                    prop.onClick  (fun _ -> dispatch (AutorisationSub Page.Chat)) 
                    prop.text "Autorise"
                    ]
                  Html.input [ 
                    prop.classes [ "input"; "is-medium" ]
                    prop.onChange (SetName >> dispatch)                    
                    ]
                  Html.span $"Your name will be {model.Name}"
               ]
            ]
Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.run