module App.Messages exposing (..)

import Material

type BoardSize = Small | Great

type Msg 
  = HumanMove (Int, Int)
  | CpuMove
  | StartGame
  | NewGame
  | UpdateBoardSize BoardSize
  | UpdatePlayer1Name String
  | UpdatePlayer2Name String
  | UpdatePlayer1ToCpu
  | UpdatePlayer2ToCpu
  | Mdl (Material.Msg Msg)

  