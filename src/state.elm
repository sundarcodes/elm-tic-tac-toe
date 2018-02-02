module State exposing (..)

import Array exposing (..)


type alias Box =
    { mark : Color
    , pos : ( Int, Int )
    }


type Color
    = Green
    | Orange
    | White


type Msg
    = Undo
    | Mark ( Int, Int )
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
                                { mark = White
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


markBoxWithColor : Color -> Array (Array Box) -> ( Int, Int ) -> Array (Array Box)
markBoxWithColor color boxes ( x, y ) =
    let
        row =
            case Array.get x boxes of
                Nothing ->
                    fromList []

                Just val ->
                    val

        box =
            case Array.get y row of
                Nothing ->
                    { mark = White
                    , pos = ( x, y )
                    }

                Just val ->
                    val
    in
        Array.set x (Array.set y { mark = color, pos = ( x, y ) } row) boxes


deduceNextMove : Color -> Color
deduceNextMove color =
    if (color == Orange) then
        Green
    else
        Orange


isGameOver : Array (Array Box) -> Bool
isGameOver arr =
    -- First check the rows
    -- Check the coloums
    -- Check the left diagonal
    -- Check the right diagonal
    False


isAllBoxSameColor : Array Box -> Bool
isAllBoxSameColor arr =
    let
        totalLength =
            Array.length arr

        numOfOranges =
            Array.length (Array.filter (\box -> box.mark == Orange) arr)

        numOfGreen =
            Array.length (Array.filter (\box -> box.mark == Green) arr)
    in
        if (totalLength == numOfOranges) || (totalLength == numOfGreen) then
            True
        else
            False


isMoveValid : Array (Array Box) -> ( Int, Int ) -> Bool
isMoveValid boxes ( x, y ) =
    let
        row =
            case Array.get x boxes of
                Nothing ->
                    fromList []

                Just val ->
                    val

        box =
            case Array.get y row of
                Nothing ->
                    { mark = White
                    , pos = ( x, y )
                    }

                Just val ->
                    val
    in
        if box.mark /= White then
            False
        else
            True


update : Msg -> Model -> Model
update msg model =
    case msg of
        Mark ( x, y ) ->
            if isMoveValid model.boxes ( x, y ) then
                { model
                    | nextMove = deduceNextMove model.nextMove
                    , boxes = markBoxWithColor model.nextMove model.boxes ( x, y )
                    , isGameOver = isGameOver model.boxes
                }
            else
                model

        Undo ->
            model

        Reset ->
            model
