module View exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (class, id)
import State exposing (Model, Box, Msg, Color)
import Array exposing (..)
import Html.Events exposing (onClick)


row : Array Box -> Html Msg
row arr =
    div [ class "row" ] (List.map (\box -> placeHolder box) (Array.toList arr))


placeHolder : Box -> Html Msg
placeHolder box =
    let
        className =
            if box.mark == State.Orange then
                "orange"
            else if box.mark == State.Green then
                "green"
            else
                ""
    in
        div
            [ class ("col-md-4 place-holder" ++ " " ++ className)
            , id (toString (Tuple.first box.pos) ++ "-" ++ toString (Tuple.second box.pos))
            , onClick (State.Mark box.pos)
            ]
            []


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ id "grid" ]
            (Array.toList (Array.map (\rowElement -> (row rowElement)) (model.boxes)))
        ]
