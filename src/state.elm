module State exposing (..)

import Array exposing (..)


type MarkerStatus
    = NotMarked
    | Color


type alias Box =
    { mark : MarkerStatus
    , pos : ( Int, Int )
    }


type Color
    = Green
    | Orange


type Msg
    = Undo
    | Mark Color
    | Reset


type alias Model =
    { boxes : Array (Array Box), isGameOver : Bool, nextMove : Color }


initialModel : Model
initialModel =
    { boxes =
        Array.fromList
            (List.map
                (\row ->
                    Array.fromList
                        ((List.map
                            (\col ->
                                { mark = NotMarked
                                , pos = ( row, col )
                                }
                            )
                            (List.range 0 2)
                         )
                        )
                )
                (List.range 0 2)
            )
    , isGameOver = False
    , nextMove = Orange
    }


update : Msg -> Model -> Model
update msg model =
    model
