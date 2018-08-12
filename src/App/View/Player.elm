module App.View.Player exposing (iconName, fieldIconName)

import App.Model.Game exposing (Color(..))
import App.Model.Player exposing (Player)

iconName : Player -> String
iconName player =
  fieldIconName player.color

fieldIconName : Color -> String
fieldIconName color =
  case color of
    Black -> "clear"
    White -> "panorama_fish_eye"