module App

open Sutil
open Sutil.DOM
open Sutil.Attr
open Feliz
open type Feliz.length

[<RequireQualifiedAccess>]
type GridItem =
    | X
    | O
    | Empty

[<RequireQualifiedAccess>]
type Player =
    | X
    | O

module Player =
    let toGridItem player =
        match player with
        | Player.X -> GridItem.X
        | Player.O -> GridItem.O
    
    let other player =
        match player with
        | Player.X -> Player.O
        | Player.O -> Player.X
    
let gridItemView item =
    match item with
    | GridItem.X -> "X"
    | GridItem.O -> "O"
    | GridItem.Empty -> " "

type Model = {
    CurrentPlayer: Player
    Grid: GridItem array
}

module Model =
    let init () = {
        CurrentPlayer = Player.O
        Grid = Array.init 9 (fun _ -> GridItem.Empty)
    }
        
    let updateGrid index (model: Model) =
        let newItem = Player.toGridItem model.CurrentPlayer
        let updateGridArray i current =
            if i = index then
                newItem
            else current
        
        let nextPlayer = Player.other model.CurrentPlayer
        let newGrid = Array.mapi updateGridArray model.Grid
        { model with Grid = newGrid; CurrentPlayer = nextPlayer }

let gridView (onClickHandler: int -> unit) index gridItem =
    Html.div [
        style [
            Css.border (length.px 1, borderStyle.solid, color.aqua)
            Css.padding (length.em 1)
            Css.custom ("aspect-ratio", "1 / 1")
        ]
        onClick (fun _ -> onClickHandler index) [Once] 
        text (gridItemView gridItem)
    ]

let view () =
    let store = Store.make (Model.init ())
    
    Html.div [
        style [
            Css.displayGrid
            Css.gridTemplateColumns [length.fr 1; length.fr 1; length.fr 1]
            Css.gridTemplateRows [length.fr 1; length.fr 1; length.fr 1]
        ]
        let model = Store.get store
        yield!
            model.Grid
            |> Array.mapi (gridView (fun i -> Store.modify (Model.updateGrid i) store))
    ]
    

view() |> mountElement "sutil-app"