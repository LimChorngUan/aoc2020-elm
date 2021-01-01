module Day11 exposing (part1and2)

import Dict exposing (Dict)


type State
    = Empty
    | Occupied
    | NA


type Direction
    = N
    | NE
    | E
    | SE
    | S
    | SW
    | W
    | NW


type alias X =
    Int


type alias Y =
    Int


part1and2 : String -> Int
part1and2 data =
    data
        |> parseSeats
        |> countOccupiedSeats


isOccupied : State -> Bool
isOccupied state =
    state == Occupied


countOccupiedSeats : Dict ( X, Y ) State -> Int
countOccupiedSeats seats =
    let
        newSeats : Dict ( X, Y ) State
        newSeats =
            List.foldl (updateSeat seats) Dict.empty (Dict.keys seats)
    in
    if Dict.isEmpty newSeats == True then
        seats
            |> Dict.values
            |> List.filter isOccupied
            |> List.length

    else
        countOccupiedSeats (Dict.union newSeats seats)


updateSeat : Dict ( X, Y ) State -> ( X, Y ) -> Dict ( X, Y ) State -> Dict ( X, Y ) State
updateSeat oldSeats ( x, y ) newSeats =
    let
        current : State
        current =
            Dict.get ( x, y ) oldSeats |> Maybe.withDefault NA
    in
    case current of
        NA ->
            newSeats

        Empty ->
            if occupiedAdjacentCount oldSeats ( x, y ) == 0 then
                Dict.insert ( x, y ) Occupied newSeats

            else
                newSeats

        Occupied ->
            if occupiedAdjacentCount oldSeats ( x, y ) >= 5 then
                Dict.insert ( x, y ) Empty newSeats

            else
                newSeats


parseSeats : String -> Dict ( X, Y ) State
parseSeats data =
    let
        parseState : String -> State
        parseState s =
            case s of
                "L" ->
                    Empty

                "#" ->
                    Occupied

                _ ->
                    NA
    in
    data
        |> String.lines
        |> List.map (String.split "" >> List.map parseState)
        |> List.indexedMap (\i xs -> List.indexedMap (\j x -> ( ( i, j ), x )) xs)
        |> List.concat
        |> Dict.fromList



-- Part 1
-- occupiedAdjacentCount : Dict ( X, Y ) State -> ( X, Y ) -> Int
-- occupiedAdjacentCount seats ( x, y ) =
--     let
--         n =
--             Dict.get ( x - 1, y ) seats |> Maybe.withDefault NA
--         ne =
--             Dict.get ( x - 1, y + 1 ) seats |> Maybe.withDefault NA
--         e =
--             Dict.get ( x, y + 1 ) seats |> Maybe.withDefault NA
--         se =
--             Dict.get ( x + 1, y + 1 ) seats |> Maybe.withDefault NA
--         s =
--             Dict.get ( x + 1, y ) seats |> Maybe.withDefault NA
--         sw =
--             Dict.get ( x + 1, y - 1 ) seats |> Maybe.withDefault NA
--         w =
--             Dict.get ( x, y - 1 ) seats |> Maybe.withDefault NA
--         nw =
--             Dict.get ( x - 1, y - 1 ) seats |> Maybe.withDefault NA
--     in
--     [ n, ne, e, se, s, sw, w, nw ]
--         |> List.filter isOccupied
--         |> List.length
-- Part 2


occupiedAdjacentCount : Dict ( X, Y ) State -> ( X, Y ) -> Int
occupiedAdjacentCount seats ( x, y ) =
    let
        n =
            isDirectionOccupied seats ( x, y ) N 1

        ne =
            isDirectionOccupied seats ( x, y ) NE 1

        e =
            isDirectionOccupied seats ( x, y ) E 1

        se =
            isDirectionOccupied seats ( x, y ) SE 1

        s =
            isDirectionOccupied seats ( x, y ) S 1

        sw =
            isDirectionOccupied seats ( x, y ) SW 1

        w =
            isDirectionOccupied seats ( x, y ) W 1

        nw =
            isDirectionOccupied seats ( x, y ) NW 1
    in
    [ n, ne, e, se, s, sw, w, nw ]
        |> List.filter (\i -> i == True)
        |> List.length


isDirectionOccupied : Dict ( X, Y ) State -> ( X, Y ) -> Direction -> Int -> Bool
isDirectionOccupied seats ( x, y ) direction n =
    let
        coordinate =
            case direction of
                N ->
                    ( x - n, y )

                NE ->
                    ( x - n, y + n )

                E ->
                    ( x, y + n )

                SE ->
                    ( x + n, y + n )

                S ->
                    ( x + n, y )

                SW ->
                    ( x + n, y - n )

                W ->
                    ( x, y - n )

                NW ->
                    ( x - n, y - n )
    in
    case Dict.get coordinate seats of
        Just state ->
            case state of
                Occupied ->
                    True

                Empty ->
                    False

                NA ->
                    isDirectionOccupied seats ( x, y ) direction (n + 1)

        Nothing ->
            False
