module App.View.Menu exposing (view)

import App.Messages exposing (Msg(UpdateBoardSize, UpdatePlayer1Name, UpdatePlayer2Name, UpdatePlayer1ToCpu, UpdatePlayer2ToCpu, StartGame, Mdl), BoardSize(..))
import App.Model as Model
import App.Model.Player as Player 
import App.View.Player as PlayerView

import Html exposing (Html, text, div)
import Html.Attributes exposing (class)

import Material
import Material.Button as Button
import Material.List as Lists
import Material.Options as Options
import Material.Textfield as Textfield
import Material.Toggles as Toggles

view : Model.Model -> Html Msg
view model =
  Lists.ul []
  [ Lists.li []
    [
      text "FIVE IN A ROW"
    ]
  , Lists.li []
      [ Lists.content [] 
        (renderRadioSwitch 
          "BoardSize"
          "apps"
          ("Small", (UpdateBoardSize Small))
          ("Great", (UpdateBoardSize Great))
          (model.menu_BoardSize == Small)
        )
      ]
  , Lists.li []
      [ Lists.content []
        (renderPlayerSwitch
          "Player1"
          model.player1
          UpdatePlayer1ToCpu
          UpdatePlayer1Name
        )
      ]
  , Lists.li []
      [ Lists.content []
        (renderPlayerSwitch
          "Player2"
          model.player2
          UpdatePlayer2ToCpu
          UpdatePlayer2Name
        )
      ]
  , Lists.li []
      [ Lists.content []
          [ Button.render Mdl
              [0]
              Material.model
              [ Options.onClick StartGame ]
              [ text "Start Game" ]
          ]
      ]
  ]

renderPlayerSwitch : String -> Player.Player -> Msg -> (String -> Msg) -> List (Html Msg)
renderPlayerSwitch groupName player msgToCpu msgUpdateName =
  (renderRadioSwitch 
    groupName
    (PlayerView.iconName player)
    ("Cpu", msgToCpu)
    ("Human", (msgUpdateName ""))
    (Player.isCpu player)) 
  ++ [ div [class "textbox"]  [renderPlayerTextBox player msgUpdateName]]


renderPlayerTextBox :  Player.Player -> (String -> Msg) -> Html Msg
renderPlayerTextBox  player message =
  case player.typeP of
    Player.Cpu ->
      Html.text (Player.toString player)
      
    Player.Human playerName -> 
      Textfield.render Mdl [0] Material.model
        [ Textfield.label (if (String.isEmpty playerName) then "Player's name" else "")
        , Options.onInput message
        , Textfield.error "This is required" |> Options.when (player.canShowError && not(Player.isValid player))
        ]
        []

renderRadioSwitch : String -> String -> (String, Msg) -> (String, Msg) -> Bool ->  List (Html Msg)
renderRadioSwitch groupName iconName (label1, msg1) (label2, msg2) value =
  [ Lists.icon iconName []
  , div [] 
    [ renderRadio groupName label1 value msg1
    , renderRadio groupName label2 (not(value)) msg2
    ]
  ]

renderRadio : String -> String -> Bool -> Msg -> Html Msg
renderRadio groupName label value msg =
  Toggles.radio Mdl [0] Material.model
    [ Toggles.value value
    , Toggles.group groupName
    , Toggles.ripple
    , Options.onToggle msg
    ]
    [ text label 
    ]