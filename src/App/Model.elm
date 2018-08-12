module App.Model exposing (Screen(..), Model, init, createNewGame, cpuPutStone, putStone, canStartGame, isGameOver, getBoardSize, currentPlayer, currentPlayerColor, getPlayer)

import App.Messages exposing (BoardSize(..))
import App.Model.Game as Game exposing (GameBoardEx, Color(..))
import App.Model.Player as Player exposing (Player)

import Material

type Screen = Menu | Game

type alias Model = 
  { screen : Screen
  , game : GameBoardEx
  , mdl : Material.Model
  , menu_BoardSize : BoardSize
  , player1 : Player
  , player2 : Player
  }

init : Model
init = 
  { screen = Menu
  , game = Game.createEmptyGame
  , mdl = Material.model
  , menu_BoardSize = Small
  , player1 = Player.createHuman Black ""
  , player2 = Player.createHuman White ""
  }

createNewGame : Model -> Model
createNewGame model =
  { model 
    | screen = Game
    , game = Game.createGame (getBoardSize model.menu_BoardSize)
    , player1 = Player.enableError model.player1
    , player2 = Player.enableError model.player2
  }

cpuPutStone : Model -> Model
cpuPutStone model =
  let
    location = Game.cpuMove Game.expert model.game
  in
    case location of
      Nothing -> model
      Just (row, col) -> putStone row col model

putStone : Int -> Int -> Model -> Model
putStone row col model =
  { model | game = Game.moveFuncEx model.game (row, col) }

canStartGame : Model -> Bool
canStartGame model =
  (Player.isValid model.player1) && (Player.isValid model.player2)

isGameOver : Model -> Bool
isGameOver model =
  Game.isGameOver model.game

getBoardSize : BoardSize -> Int
getBoardSize boardSize =
  case boardSize of
    Small -> 9
    Great -> 17

currentPlayer : Model -> Player
currentPlayer model =
  currentPlayerColor model |> getPlayer model

currentPlayerColor : Model -> Color
currentPlayerColor model =
  Game.player model.game
  
getPlayer : Model -> Color -> Player
getPlayer model color =
  case color of
    Black -> model.player1
    White -> model.player2