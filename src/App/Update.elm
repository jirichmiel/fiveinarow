module App.Update exposing (update)

import Material
import Process
import Task
import Time exposing (millisecond)

import App.Model exposing (Model, Screen(..))
import App.Model.Player as Player
import App.Messages exposing (..)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of

    HumanMove (row, col) ->
      App.Model.putStone row col model 
        |> triggerComputerMove

    CpuMove ->
      App.Model.cpuPutStone model
        |> triggerComputerMove

    StartGame ->
      if (App.Model.canStartGame model) then
        App.Model.createNewGame model 
          |> triggerComputerMove
      else
        ( { model | player1 = model.player1 |> Player.enableError, player2 = model.player2 |> Player.enableError }
        , Cmd.none
        )
    
    NewGame ->
      { model | screen = Menu }
        |> triggerComputerMove

    UpdateBoardSize size ->
      ( { model | menu_BoardSize = size }
      , Cmd.none
      )

    UpdatePlayer1Name name ->
      ( { model | player1 = model.player1 |> Player.changeToHuman name |> Player.enableError }
      , Cmd.none
      )

    UpdatePlayer2Name name ->
      ( { model | player2 = model.player2 |> Player.changeToHuman name |> Player.enableError }
      , Cmd.none
      )

    UpdatePlayer1ToCpu ->
      ( { model | player1 = model.player1 |> Player.changeToCpu |> Player.disableError }
      , Cmd.none
      )

    UpdatePlayer2ToCpu ->
      ( { model | player2 = model.player2 |> Player.changeToCpu |> Player.disableError }
      , Cmd.none
      )

    Mdl msg_ ->
      Material.update Mdl msg_ model

triggerComputerMove : Model -> (Model, Cmd Msg)
triggerComputerMove model =
  (model, playerCmd model)

playerCmd : Model -> Cmd Msg
playerCmd model =
  if (App.Model.isGameOver <| model) then
    Cmd.none
  else if (Player.isCpu <| App.Model.currentPlayer model) then 
    Task.perform (\_ -> CpuMove) (Process.sleep (50 * millisecond))
  else
    Cmd.none