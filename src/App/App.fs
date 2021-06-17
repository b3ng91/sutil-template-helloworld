module App

open Sutil
open Sutil.DOM
open Sutil.Attr
open Feliz
open type Feliz.length

type GridItem =
    | X
    | O
    | Empty

let gridItems =
    Array.init 9 (fun i -> (i, GridItem.Empty))

let view() =
    Html.div [
        style [
            Css.displayGrid
            Css.gridTemplateColumns [length.fr 1; length.fr 1; length.fr 1]
            Css.gridTemplateRows [length.fr 1; length.fr 1; length.fr 1]
        ]
        for (i, gridItem) in gridItems do
            Html.div [
                style [
                    Css.border (length.px 1, borderStyle.solid, color.aqua)
                    Css.padding (length.em 1)
                    Css.custom ("aspect-ratio", "1 / 1")
                ]
                text (string (1 + i))
            ]
    ]
    

view() |> mountElement "sutil-app"