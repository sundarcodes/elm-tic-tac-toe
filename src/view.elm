module View exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (class, id)
import State exposing (Model, Box, Msg, Color)
import Array exposing (..)
import Html.Events exposing (onClick)


row : Array Box -> Color -> Html Msg
row arr nextMove =
    div [ class "row" ] (List.map (\box -> placeHolder box nextMove) (Array.toList arr))


placeHolder : Box -> Color -> Html Msg
placeHolder box nextMove =
    div
        [ class "col-md-4 place-holder"
        , id (toString (Tuple.first box.pos) ++ "-" ++ toString (Tuple.second box.pos))
        , onClick (State.Mark nextMove)
        ]
        []


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ id "grid" ]
            (Array.toList (Array.map (\rowElement -> row rowElement model.nextMove) (model.boxes)))
        ]
