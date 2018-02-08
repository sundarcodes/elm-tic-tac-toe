module State exposing (..)

import Dict exposing (..)
import Array exposing (Array)


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
    | Redo
    | Mark ( Int, Int )
    | Reset


type GameStatus
    = NotStarted
    | InProgress
    | Drawn
    | Won MarkerType


type alias Board =
    Dict ( Int, Int ) Box


type alias GameState =
    { boxes : Board
    , gameStatus : GameStatus
    , nextTurn : MarkerType
    }


type alias Model =
    { gameState : GameState
    , previousStates : Array GameState
    , nextStates : Array GameState
    }


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


initializeGameState : GameState
initializeGameState =
    { boxes = buildInitialBoard
    , gameStatus = NotStarted
    , nextTurn = Cross
    }


initialModel : Model
initialModel =
    { gameState = initializeGameState
    , previousStates = Array.fromList []
    , nextStates = Array.fromList []
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


isGameOver : Board -> MarkerType -> GameStatus
isGameOver board currentTurn =
    -- First check the rows
    if isAllRowsMarked board then
        Won (findNextTurn currentTurn)
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


isGameDone : GameStatus -> Bool
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


updateMarkers : GameState -> ( Int, Int ) -> GameState
updateMarkers gameState pos =
    { gameState
        | nextTurn = findNextTurn gameState.nextTurn
        , boxes = markBoxWithMarker gameState.nextTurn gameState.boxes pos
    }


updateMarkersState : ( Int, Int ) -> Model -> Model
updateMarkersState pos model =
    { model
        | gameState = updateMarkers model.gameState pos
    }


updateGameState : Model -> Model
updateGameState model =
    { model
        | gameState = updateGameStatus model.gameState
    }


updateGameStatus : GameState -> GameState
updateGameStatus gameState =
    { gameState
        | gameStatus = isGameOver gameState.boxes gameState.nextTurn
    }


pushGameStateToUndoList : Model -> Model
pushGameStateToUndoList model =
    { model
        | previousStates = (Array.push model.gameState model.previousStates)
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Mark ( x, y ) ->
            if isMoveValid model.gameState.boxes ( x, y ) && (not (isGameDone model.gameState.gameStatus)) then
                -- Debug.log (toString model)
                pushGameStateToUndoList model
                    |> updateMarkersState ( x, y )
                    |> updateGameState
            else
                model

        Undo ->
            let
                arrLength =
                    Array.length model.previousStates

                previousGameState =
                    case Array.get (arrLength - 1) model.previousStates of
                        Nothing ->
                            model.gameState

                        Just val ->
                            val
            in
                { model
                    | gameState = previousGameState
                    , previousStates = Array.slice 0 -1 model.previousStates
                    , nextStates = Array.push previousGameState model.nextStates
                }

        Redo ->
            model

        Reset ->
            initialModel
