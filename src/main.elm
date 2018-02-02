module Main exposing (..)

import Html exposing (beginnerProgram)
import State exposing (Model, update, initialModel, Msg)
import View exposing (view)


main : Program Never Model Msg
main =
    beginnerProgram { model = initialModel, view = view, update = update }
