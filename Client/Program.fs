module Program
open Shared
open Fable.Remoting.Client
open Elmish
open Elmish.React
open Feliz

let carsStore : ICarsStore = 
  Remoting.createApi()
  |> Remoting.buildProxy<ICarsStore>


async {
    let! allcars = carsStore.carsfrombase
    for carr in allcars do
        printfn "%s (%s)" carr.Name carr.Url
} 
|> Async.StartImmediate

type Model = { Cars: Car list  }   

type Msg =
    | ShowCars of Car list
    | ShowImage 
    | GetSome

let dispatcher = 
    fun dispatch ->
        async {let! allcars = carsStore.carsfrombase
               dispatch (ShowCars allcars)  } |> Async.StartImmediate

let init() = { Cars = []}, Cmd.none  

let update msg model =
    match msg with
    | ShowCars x       -> { model with Cars = x }, Cmd.none
    | ShowImage        -> model, [dispatcher]
    | GetSome              -> model, Cmd.none

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
              let reversed = model.Cars|> List.rev
              for chat in reversed ->
               div [] [
                Html.img [
                  prop.src chat.Url
                ]
                Html.li [
                  prop.classes ["box"; "subtitle"]
                  prop.text chat.Name
                ]
               ]
            ]
          ]
        ]
      ]
    ]

let buttonField (model: Model) (dispatch: Msg -> unit) =
  div [ "field"; "has-addons" ] [
      div ["control"] [
          Html.button [
            prop.classes [ "button"; "is-primary"; "is-medium" ]
            prop.onClick (fun _ -> dispatch GetSome)
            prop.children [
              Html.i [ prop.classes [ "fa"; "fa-plus" ] ]
            ]
          ]
          Html.button [
            prop.classes [ "button"; "is-primary"; "is-medium" ]
            prop.onClick (fun _ -> dispatch ShowImage)
            prop.children [
              Html.i [ prop.classes [ "fa"; "fa-plus" ] ]
            ]
          ]
      ]       
  ] 
      
let view (model: Model) dispatch =
        Html.div [
          prop.style [style.padding 20]
          prop.children [
            appTitle
            buttonField model dispatch         
            renderLists model dispatch          
          ]     
        ]  
        
Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.run