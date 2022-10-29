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

type State = {Subscribers:WebSocket list}

type Msg =
    | SendAll of ByteSegment
    | Subscribe of WebSocket

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
            let! _, input, _ = webSocket.read()
            let text = ASCII.toString input
            let byteResponse =
                text
                |> System.Text.Encoding.ASCII.GetBytes
                |> ByteSegment
//            do! webSocket.send Text byteResponse true
            printfn $"{byteResponse}"
            processor.Post(SendAll byteResponse )
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