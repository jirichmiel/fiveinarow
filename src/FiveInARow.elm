import Html exposing (program)

import App.Messages exposing (Msg)
import App.Model exposing (Model, init)
import App.Update exposing (update)
import App.View exposing (view)

main : Program Never Model Msg
main =
  Html.program
    { init = ( init, Cmd.none )
    , view = view
    , subscriptions = always Sub.none
    , update = update
    }