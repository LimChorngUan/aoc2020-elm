module Day15 exposing (getNumber)

import Dict exposing (Dict)


type alias GameRecord =
    Dict Number ( Maybe Turn, Turn )


type alias Number =
    Int


type alias Turn =
    Int


getNumber : String -> Number
getNumber data =
    let
        initNums =
            String.split "," data |> List.filterMap String.toInt

        initTurn =
            List.length initNums

        initPrevNum =
            initNums |> List.reverse |> List.head |> Maybe.withDefault -1

        initGameRecord =
            initNums
                |> List.indexedMap (\i n -> Tuple.pair n ( Nothing, i ))
                |> Dict.fromList
    in
    playGame initTurn initPrevNum initGameRecord


playGame : Turn -> Number -> GameRecord -> Number
playGame turn prevNum gameRecord =
    let
        currentNum : Number
        currentNum =
            getCurrentTurnNum turn prevNum gameRecord
    in
    -- Part 1
    -- if turn == 2020 - 1 then
    -- Part 2
    if turn == 30000000 - 1 then
        -- Just wait abit longer :p
        currentNum

    else
        playGame (turn + 1) currentNum (updateGameRecord currentNum turn gameRecord)


getCurrentTurnNum : Turn -> Number -> GameRecord -> Number
getCurrentTurnNum turn prevNum game =
    let
        lastTurnsOfPrevNum : ( Maybe Turn, Turn )
        lastTurnsOfPrevNum =
            Dict.get prevNum game |> Maybe.withDefault ( Nothing, -1 )
    in
    case lastTurnsOfPrevNum of
        ( Nothing, y ) ->
            0

        ( Just x, y ) ->
            y - x


updateGameRecord : Number -> Turn -> GameRecord -> GameRecord
updateGameRecord num turn game =
    let
        turns : Maybe ( Maybe Turn, Turn )
        turns =
            Dict.get num game
    in
    case turns of
        Nothing ->
            Dict.insert num ( Nothing, turn ) game

        Just ( _, lastTurn ) ->
            Dict.insert num ( Just lastTurn, turn ) game
