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

type State = {Subscribers:WebSocket list}

type Msg =
    | SendAll of ByteSegment
    | Subscribe of WebSocket
    | Unsubscribe of WebSocket

let processor = MailboxProcessor<Msg>.Start(fun inbox ->
    let rec innerLoop state  = async {
        let! message = inbox.Receive()
        match message with
        | SendAll msg -> 
//            state.Subscribers|>List.iter (fun x ->  x.send Text msg true)
            for x in state.Subscribers do
                let! result = x.send Text msg true
                ()              
            do! innerLoop state
        | Subscribe ws ->
            let state = { state with Subscribers = ws::state.Subscribers }  
            do! innerLoop state
        | Unsubscribe ws -> 
            let state = { state with Subscribers = state.Subscribers|> List.filter (fun x -> x <> ws) } 
            do! innerLoop state
        ()
         }

    innerLoop {Subscribers=[]})


let ipAdress = Dns.GetHostEntry(Dns.GetHostName()).AddressList|>Seq.find (fun x -> x.AddressFamily = AddressFamily.InterNetwork)
let stringIpAdress = ipAdress.ToString()



let ws (webSocket : WebSocket) _ =
    processor.Post(Subscribe webSocket)
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
            | Autorise -> printfn "u are the Best!"
        | (Close, input, _) -> 
            processor.Post (Unsubscribe webSocket)
            loop <- false
        | _ -> ()
           }
        
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