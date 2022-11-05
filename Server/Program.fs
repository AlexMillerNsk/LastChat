module Server

open System.Net
open System.IO
open Suave
open Suave.Http
open Suave.Sockets.Control
open Suave.Utils
open Filters
open Operators
open WebSocket
open Suave.Sockets
open System.Net.Sockets
open Shared
open FSharp.Json

type Subscriber = WebSocket * string

type State = {Subscribers:Subscriber list}

type Msg =
    | SendAll of ByteSegment
    | Subscribe of Subscriber
    | Unsubscribe of WebSocket

let msgMake x = x|>Json.serialize|> System.Text.Encoding.ASCII.GetBytes|> ByteSegment

let processor = MailboxProcessor<Msg>.Start(fun inbox ->
    let rec innerLoop state  = async {
        let! message = inbox.Receive()
        match message with
        | SendAll msg ->            
            for x in state.Subscribers do
                let ws = fst x
                let! result = ws.send Text msg true
                printfn "SendAll"
                ()              
            do! innerLoop state
        | Subscribe (x:Subscriber) ->
            let state = { state with Subscribers = x::state.Subscribers } 
            printfn "Subscribe"
            let name = snd x
            let msg = { MsgType = AutorisationType OpenAutorisation ; Message = name}|>msgMake
            SendAll msg|>ignore
            
            ()
            do! innerLoop state
        | Unsubscribe ws -> 
            let name = state.Subscribers|>List.find (fun (x,y) -> x = ws)|>snd 
            let msg = { MsgType = AutorisationType ClosedAutorisation ; Message = name}|>msgMake
            SendAll msg|>ignore
            let state = { state with Subscribers = state.Subscribers|>List.filter (fun (x,y) -> x <> ws) }
            printfn "Unsubscribe"
            ()  
            do! innerLoop state
        ()
         }

    innerLoop {Subscribers=[]})


let ipAdress = Dns.GetHostEntry(Dns.GetHostName()).AddressList|>Seq.find (fun x -> x.AddressFamily = AddressFamily.InterNetwork)
let stringIpAdress = ipAdress.ToString()

let closedWsMsg = 
    let msg = { MsgType = AutorisationType ClosedAutorisation ; Message = ""}
    let serializedMsg = Json.serialize msg
    serializedMsg|> System.Text.Encoding.ASCII.GetBytes
                    

let ws (webSocket : WebSocket) _ =
    processor.Post(Subscribe (webSocket,""))
    socket {
        let mutable loop = true
        
        while loop do
        let! msg = webSocket.read()
        match msg with
        | (Text, input, _) ->
            let text = ASCII.toString input 
            let deserializedText = Json.deserialize<WsMessage> text
            match deserializedText.MsgType with
            | SendMessage ->
                let byteResponse =
                    deserializedText.Message
                    |> System.Text.Encoding.ASCII.GetBytes
                    |> ByteSegment
                printfn $"{text}"
                processor.Post(SendAll byteResponse )
                ()
            | AutorisationType autorisationStatus ->
                match autorisationStatus with
                | OpenAutorisation -> 
                    let name = deserializedText.Message
                    let newSubscriber:Subscriber = (webSocket,name)
                    processor.Post(Subscribe newSubscriber)
                    printfn $"{newSubscriber}"                   
                | _ -> printfn "error"



        | (Close, input, _) -> 
            printfn "good bye boi"
            processor.Post (Unsubscribe webSocket)
            processor.Post (SendAll (input|> ByteSegment ))
            let text = ASCII.toString input 
            printfn $"{text}"
            loop <- false
        | _ -> ()
           }
let result State = State.Subscribers|>List.map (fun (x,y) -> y.ToString())

let app: WebPart =
     choose [
          GET >=> path "/" >=> Files.file "./public/index.html"    
          GET >=> path "/test" >=> Successful.OK "Biden666"
          GET >=> Files.browseHome
          path "/websocket" >=> handShake ws
          RequestErrors.NOT_FOUND "Page not found." ]

let config = {
    defaultConfig with
        bindings = [HttpBinding.createSimple Protocol.HTTP stringIpAdress 8080]
        homeFolder = Some(Path.GetFullPath "./public")
}

startWebServer config app
