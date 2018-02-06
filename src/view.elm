module View exposing (..)

import Html exposing (Html, div, text, button)
import Html.Attributes exposing (class, id)
import State exposing (Model, Box, Msg, MarkerType, Board)
import Dict exposing (..)
import Html.Events exposing (onClick)


row : List Box -> Html Msg
row arr =
    div [ class "row" ] (List.map (\box -> placeHolder box) (arr))


placeHolder : Box -> Html Msg
placeHolder box =
    let
        marker =
            if box.mark == State.Cross then
                "X"
            else if box.mark == State.Circle then
                "O"
            else
                ""
    in
        div
            [ class ("col-md-4 place-holder")
            , id (toString (Tuple.first box.pos) ++ "-" ++ toString (Tuple.second box.pos))
            , onClick (State.Mark box.pos)
            ]
            [ text marker ]


viewRow : Int -> Board -> Html Msg
viewRow rowNum board =
    row
        ((Dict.filter (\( x, y ) val -> x == rowNum) board)
            |> Dict.values
        )


viewButtons : Html Msg
viewButtons =
    div [ class "button-group" ]
        [ button [ (onClick State.Undo) ] [ text "Undo" ] ]


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ id "grid" ]
            (List.map
                (\rowNum -> viewRow rowNum model.boxes)
                (List.range 0 2)
            )
        ]
