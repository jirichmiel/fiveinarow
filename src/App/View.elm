module App.View exposing (view)

import App.Model exposing (..)
import App.Model.Game as Game exposing (..)
import App.Model.Player as Player exposing (Player)
import App.Messages exposing (Msg(StartGame, NewGame, Mdl))
import App.View.Menu exposing (view)
import App.View.GameBoard exposing (view)
import App.View.Player as PlayerView

import Html exposing (Html, text, h4, node, div)
import Html.Attributes exposing (attribute, id, class)

import Material
import Material.Scheme
import Material.Icon as Icon
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Grid exposing (grid, cell, size, offset, Device(..))

view : Model -> Html Msg 
view model =
  Html.div [id "fives"] [stylesheet, innerView model]

innerView : Model -> Html Msg
innerView model =
  case model.screen of
    Menu -> Material.Scheme.top <| (Html.div [id "menu"] [App.View.Menu.view model])
    Game -> Material.Scheme.top <| (Html.div [id "game"] [renderGameButtons, (renderGame model)])

stylesheet : Html Msg
stylesheet =
    let
        tag = "link"
        attrs =
            [ attribute "rel"       "stylesheet"
            , attribute "property"  "stylesheet"
            , attribute "href"      "styles.css"
            ]
        children = []
    in 
        node tag attrs children

renderGame : Model -> Html Msg
renderGame model =
  let
    gameState = Game.gameState model.game
    currentPlayer = getPlayer model (currentPlayerColor model)
  in
  (grid []
    [ cell [ size All 1 ]
        (renderPlayerStatus model.player1 (model.player1 == currentPlayer) gameState)

    , cell [ size All 8 ]
        (App.View.GameBoard.view model)
        
    , cell [ size All 1 ]
        (renderPlayerStatus model.player2 (model.player2 == currentPlayer) gameState)

    ])

renderPlayerStatus : Player -> Bool -> GameState -> List (Html Msg)
renderPlayerStatus player isOnMove gameState =
  [
    div [ class (playerStatusClass player.color gameState) ]
    [ iconToggle (PlayerView.iconName player) isOnMove
    , h4 [] [text (Player.toString player) ]
    , renderPlayerIconStatus player.color gameState
    ]
  ]

playerStatusClass : Color -> GameState -> String
playerStatusClass color gameState =
  case gameState of
    GameInProgress -> "status"
    Draw -> "status"
    Winner winnerColor _ -> 
      if (winnerColor == color) then
        "status sentiment_very_satisfied"
      else
        "status sentiment_very_dissatisfied"

renderPlayerIconStatus : Color -> GameState -> Html Msg
renderPlayerIconStatus color gameState =
  case gameState of
    GameInProgress -> Html.text ""
    Draw -> icon "sentiment_neutral"
    Winner winnerColor _ -> 
      if (winnerColor == color) then
        icon "sentiment_very_satisfied"
      else
        icon "sentiment_very_dissatisfied"

icon : String -> Html Msg
icon iconName =
  iconToggle iconName False

iconToggle : String -> Bool -> Html Msg
iconToggle iconName value =
  iconBackColor iconName (if value then "lightblue" else "none")

iconBackColor : String  -> String -> Html Msg
iconBackColor iconName backColor =
  Icon.view iconName [ Icon.size48, css "background" backColor ] 

renderGameButtons : Html Msg
renderGameButtons =
  Html.div [] 
    [ renderButton "Restart" StartGame
    , renderButton "New Game" NewGame
    ]

renderButton : String -> Msg -> Html Msg
renderButton label msg =
   Button.render Mdl [0] Material.model [ Options.onClick msg ] [ text label ]

