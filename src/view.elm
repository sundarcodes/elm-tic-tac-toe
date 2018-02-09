module View exposing (..)

import Html exposing (Html, div, text, button, span, h1)
import Html.Attributes exposing (class, id)
import State exposing (Model, Box, Msg, MarkerType, Board, GameStatus)
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
            [ class ("place-holder")
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
        [ button [ class "btn", (onClick State.Undo) ] [ text "Undo" ]
        , button [ class "btn", (onClick State.Reset) ] [ text "Reset" ]
        ]


viewInfo : Model -> Html Msg
viewInfo model =
    div [ class "information-board" ]
        [ div []
            [ text ("Game Status: ")
            , span [ class "game-status" ] [ text (mapModelGameStatusToViewName model.gameState.gameStatus) ]
            ]
        , div []
            [ text ("Next Turn: ")
            , span [ class "next-turn" ] [ text (mapModelPlayerTypeToView model.gameState.nextTurn) ]
            ]
        ]


viewHeader : Html Msg
viewHeader =
    div []
        [ h1 [] [ text "Elmy Tic Tac Toe" ] ]


view : Model -> Html Msg
view model =
    div [ class "container-board" ]
        [ viewHeader
        , viewInfo model
        , div [ class "marker-board" ]
            (List.map
                (\rowNum -> viewRow rowNum model.gameState.boxes)
                (List.range 0 2)
            )
        , viewButtons
        ]


mapModelGameStatusToViewName : State.GameStatus -> String
mapModelGameStatusToViewName gameStatus =
    case gameStatus of
        State.NotStarted ->
            "Not Started"

        State.Drawn ->
            "Match Drawn"

        State.Won player ->
            "Won by " ++ mapModelPlayerTypeToView player

        State.InProgress ->
            "Play in Progress"


mapModelPlayerTypeToView : State.MarkerType -> String
mapModelPlayerTypeToView playerType =
    case playerType of
        State.Circle ->
            "O"

        State.Cross ->
            "X"

        State.None ->
            ""
