namespace Shared

type AutorisationStatus = 
    |OpenAutorisation 
    |ClosedAutorisation

type MsgType =
    | SendMessage
    | AutorisationType of AutorisationStatus


type WsMessage =
    { MsgType: MsgType ; Message: string}
     static member Default = { MsgType = SendMessage; Message = "" }


