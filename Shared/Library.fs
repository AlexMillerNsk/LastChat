namespace Shared

type MsgType =
    | SendMessage
    | Autorise

type WsMessage =
    { MsgType: MsgType ; Message: string}
     static member Default = { MsgType = Autorise; Message = "" }


