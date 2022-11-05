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
               Subscribers: string list}


type Msg =
    | SentRequest
    | SetTextBox of string
    | SetName of string
    | SetResponse of string
    | SetChat of string
    | AutorisationSub of Page
    | SetSubscribers of string
    | DeleteSubscribers of string



let webSocket = WebSocket.Create($"ws://192.168.0.170:8080/websocket")

let closed = webSocket.addEventListener_close

let chatDecoder x = 
    let deserializedText = Json.parseAs<WsMessage> x
    match deserializedText.MsgType with
    | AutorisationType OpenAutorisation -> SetSubscribers (deserializedText.Message)
    | AutorisationType ClosedAutorisation -> DeleteSubscribers (deserializedText.Message)
    | SendMessage -> SetChat (deserializedText.Message)


let registerOnMessageHandler =
    fun dispatch ->
        async {
            webSocket.addEventListener_message (fun xy -> dispatch <| chatDecoder (xy.data.ToString()))
        } |> Async.StartImmediate

let init() = { State = "test1"
               TextBox = ""
               Name = ""
               Content = ["testcontent"]
               CurrentPage = Page.Autorisation
               Subscribers =["tester"]}, [ registerOnMessageHandler ]

    

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
    | SentRequest    ->   model, [ sendRequest model ]
    | SetTextBox v   -> { model with TextBox = v }, Cmd.none
    | SetResponse r  -> { model with State = r }, Cmd.none
    | SetChat x      -> { model with
                            TextBox = ""
                            Content = x::model.Content
                            }, Cmd.none
    | AutorisationSub page -> {model with CurrentPage = page}, [sendName model]
    | SetName x      -> {model with Name = x}, Cmd.none
    | SetSubscribers x -> {model with Subscribers = x::model.Subscribers}, Cmd.none
    | DeleteSubscribers name -> {model with Subscribers = 
                                      let newList = model.Subscribers|> List.filter (fun x -> x <> name)
                                      newList}, Cmd.none

let appTitle =
  Html.p [
    prop.className "title"
    prop.text "Welcome to our Awesome Chat!"
  ]

//let subscribersList (model: Model) (dispatch: Msg -> unit) =
//  Html.ul [
//    prop.children [
//      for subscriber in model.Subscribers ->
//        Html.li [
//          prop.classes ["box"; "subtitle"]
//          prop.text subscriber
//        ]
//    ]
//  ]
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
            prop.children [
              for subscriber in model.Subscribers ->
                Html.li [
                  prop.classes ["box"; "subtitle"]
                  prop.text subscriber
                ]
            ]
          ]
        ]
      ]
    ]
  ]

//let chatList (model: Model) (dispatch: Msg -> unit) =
//  Html.ul [
//    prop.children [
//      let reversed = model.Content|> List.rev
//      for chat in reversed ->
//        Html.li [
//          prop.classes ["box"; "subtitle"]
//          prop.text chat
//        ]
//    ]
//  ]

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
//|> Program.withSubscription onMessage 
|> Program.withReactSynchronous "elmish-app"
|> Program.run