module App.Model.Player exposing (..)

import App.Model.Game exposing (Color(..))

type PlayerType = Cpu | Human String

type alias Player = 
  { color : Color
  , typeP : PlayerType
  , canShowError : Bool
}

createHuman : Color -> String -> Player
createHuman color string =
  { color = color
  , typeP = Human string
  , canShowError = False
  }

createCpu : Color -> String -> Player
createCpu color string =
  { color = color
  , typeP = Human string
  , canShowError = False
  }

changeToCpu : Player -> Player
changeToCpu player =
  { player | typeP = Cpu }

changeToHuman : String -> Player -> Player
changeToHuman name player =
  { player | typeP = Human name }

enableError : Player -> Player
enableError player = 
  { player | canShowError = True }

disableError : Player -> Player
disableError player =
  { player | canShowError = False }

toString : Player -> String
toString player =
  case player.typeP of
    Cpu -> "CPU"
    Human name -> name

isValid : Player -> Bool
isValid player =
  case player.typeP of
    Cpu -> True
    Human name -> not (String.isEmpty name)

isCpu : Player -> Bool
isCpu player =
  case player.typeP of
    Cpu -> True
    Human _ -> False
