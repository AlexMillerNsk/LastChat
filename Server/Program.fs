module Server

open System
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
open Suave.Successful
open System.Threading

 
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
            let listNames = state.Subscribers|>List.map (fun (x,y) -> y)|>string
            let msg = { MsgType = AutorisationType OpenAutorisation ; Message = listNames}|>msgMake           
            printfn $"{listNames}"
            for subscriber in state.Subscribers do
                let ws = fst subscriber
                let! result = ws.send Text msg true
                printfn "Send from Subscribe"
                ()    
            ()
            do! innerLoop state
        | Unsubscribe ws -> 
            let state = { state with Subscribers = state.Subscribers|>List.filter (fun (x,y) -> x <> ws) }
            let listNames = state.Subscribers|>List.map (fun (x,y) -> y)|>string
            let msg = { MsgType = AutorisationType ClosedAutorisation ; Message = listNames}|>msgMake
            printfn $"{listNames}"
            for subscriber in state.Subscribers do
                let wss = fst subscriber
                let! result = wss.send Text msg true
                printfn "Send from Unubscribe"
                ()   
            ()  
            do! innerLoop state
        ()
         }

    innerLoop {Subscribers=[]})

let ipAdress = Dns.GetHostEntry(Dns.GetHostName()).AddressList|>Seq.find (fun x -> x.AddressFamily = AddressFamily.InterNetwork)
let stringIpAdress = string ipAdress
                    
let ws (webSocket : WebSocket) _ =
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
                    text
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
[<EntryPoint>]
let main argv =
    let app: WebPart =
         choose [
              GET >=> path "/" >=> Files.browseFileHome "index.html"    
              GET >=> path "/test" >=> Successful.OK "Biden666"
              GET >=> Files.browseHome
              path "/websocket" >=> handShake ws
              RequestErrors.NOT_FOUND "Page not found." 
              POST >=> path "/hello" >=> OK "Hello POST"]  

    let portEnvVar = Environment.GetEnvironmentVariable "PORT"
    let port = if String.IsNullOrEmpty portEnvVar then 8080 else (int)portEnvVar
    let cts = new CancellationTokenSource()
    let config = {
        defaultConfig with
            cancellationToken = cts.Token
            bindings = [HttpBinding.createSimple HTTP stringIpAdress port]
            homeFolder = Some(Path.GetFullPath "../Server/Public")
    }
    let f = Path.GetFullPath "../Server/Public"
    printfn $"{f}"
    startWebServer config app
    0
