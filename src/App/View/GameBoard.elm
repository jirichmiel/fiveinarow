module App.View.GameBoard exposing (view)

import App.Model exposing (Model)
import App.Model.Game as Game exposing (GameState(..), Field)
import App.Messages exposing (Msg(HumanMove, Mdl))
import App.View.Player as PlayerView

import Html exposing (Html, br)

import Material
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Icon as Icon

view : Model -> List (Html Msg)
view model =
  Game.mapBoardFields model.game (renderGameField model)
    |> List.concat

renderGameField : Model -> (Int, Int) -> Field -> List (Html Msg)
renderGameField model location field =
  let
    fieldHtml =
        case field of
          Game.Empty ->
            Button.render Mdl
              [ 0 ]
              Material.model
              [ Button.minifab
              , Button.colored
              , onClickHandler model location field
              ]
              [ Icon.view "apps" [ Icon.size18 ] ]

          Game.Stone c ->
            Button.render Mdl
              [ 0 ]
              Material.model
              [ Button.minifab
              , Button.colored
              , onClickHandler model location field
              ]
              [ Icon.view (PlayerView.fieldIconName c) [ Icon.size18, css "background" (backColor model location) ] ]
  in
    if (isStartRow location) then 
      [ br [] [], fieldHtml ]
    else
      [ fieldHtml ]


onClickHandler : Model -> (Int, Int) -> Field -> Button.Property Msg 
onClickHandler model location field =
  if (Game.isGameOver model.game || field /= Game.Empty) then
    Button.disabled
  else
    Options.onClick (HumanMove location)

backColor : Model -> (Int, Int) -> String
backColor model location = 
  if (Game.isWinnerLocation model.game location) then
    "yellow"
  else
    "none"

isStartRow : (Int, Int) -> Bool
isStartRow (row, col) =
  col == 0