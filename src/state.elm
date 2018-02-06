module State exposing (..)

import Dict exposing (..)


type alias Box =
    { mark : MarkerType
    , pos : ( Int, Int )
    }


type MarkerType
    = Cross
    | Circle
    | None


type Msg
    = Undo
    | Mark ( Int, Int )
    | Reset


type GameState
    = NotStarted
    | InProgress
    | Drawn
    | Won MarkerType


type alias Board =
    Dict ( Int, Int ) Box


type alias Model =
    { boxes : Board, gameState : GameState, nextTurn : MarkerType }


flatten2D : List (List a) -> List a
flatten2D list =
    List.foldr (++) [] list


buildInitialBoard : Board
buildInitialBoard =
    Dict.fromList
        ((List.map
            (\row ->
                (List.map
                    (\col ->
                        ( ( row, col )
                        , { mark = None
                          , pos = ( row, col )
                          }
                        )
                    )
                    (List.range
                        0
                        2
                    )
                )
            )
            (List.range
                0
                2
            )
         )
            |> flatten2D
        )


initialModel : Model
initialModel =
    { boxes = buildInitialBoard
    , gameState = NotStarted
    , nextTurn = Cross
    }


markBoxWithMarker : MarkerType -> Board -> ( Int, Int ) -> Board
markBoxWithMarker marker board ( x, y ) =
    Dict.update ( x, y )
        (\val ->
            let
                res =
                    case val of
                        Nothing ->
                            { mark = None, pos = ( -1, -1 ) }

                        Just val ->
                            { val | mark = marker }
            in
                Just res
        )
        board


findNextTurn : MarkerType -> MarkerType
findNextTurn turn =
    if (turn == Cross) then
        Circle
    else
        Cross


isGameOver : Board -> MarkerType -> GameState
isGameOver board currentTurn =
    -- First check the rows
    if isAllRowsMarked board then
        Won currentTurn
    else
        InProgress



-- Check the coloums
-- Check the left diagonal
-- Check the right diagonal


isAllRowsMarked : Board -> Bool
isAllRowsMarked board =
    let
        rowResults =
            List.map (\rowNum -> isAllBoxesinRowMarked board rowNum) (List.range 0 2)
    in
        List.member True rowResults


isAllBoxesinRowMarked : Board -> Int -> Bool
isAllBoxesinRowMarked board rowNum =
    Dict.filter (\( x, y ) val -> x == rowNum) board
        |> Dict.values
        |> isAllBoxSameColor


isAllBoxSameColor : List Box -> Bool
isAllBoxSameColor arr =
    let
        totalLength =
            List.length arr

        numOfCircles =
            List.length (List.filter (\box -> box.mark == Circle) arr)

        numOfCrosses =
            List.length (List.filter (\box -> box.mark == Cross) arr)
    in
        if (totalLength == numOfCircles) || (totalLength == numOfCrosses) then
            True
        else
            False


isMoveValid : Board -> ( Int, Int ) -> Bool
isMoveValid board ( x, y ) =
    let
        box =
            Dict.get ( x, y ) board
    in
        case box of
            Nothing ->
                False

            Just val ->
                if (val.mark == None) then
                    True
                else
                    False


isGameDone : GameState -> Bool
isGameDone state =
    case state of
        InProgress ->
            False

        Won player ->
            True

        Drawn ->
            True

        NotStarted ->
            False


updateMarker : Model -> ( Int, Int ) -> Model
updateMarker model pos =
    { model
        | nextTurn = findNextTurn model.nextTurn
        , boxes = markBoxWithMarker model.nextTurn model.boxes pos
    }


updateGameState : Model -> Model
updateGameState model =
    { model
        | gameState = isGameOver model.boxes model.nextTurn
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Mark ( x, y ) ->
            if isMoveValid model.boxes ( x, y ) && (not (isGameDone model.gameState)) then
                Debug.log (toString model)
                    updateMarker
                    model
                    ( x, y )
                    |> updateGameState
            else
                model

        Undo ->
            model

        Reset ->
            model
