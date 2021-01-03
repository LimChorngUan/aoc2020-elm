module Day13 exposing (part1, part2)

import Parser exposing ((|.), (|=), Parser)


type alias BusId =
    Int


type alias Delay =
    Int


part2 : String -> Int
part2 data =
    let
        schedules : List ( Delay, BusId )
        schedules =
            data
                |> String.lines
                |> List.drop 1
                |> List.head
                |> Maybe.withDefault ""
                |> String.split ","
                |> List.indexedMap Tuple.pair
                |> List.filter (\( delay, id ) -> id /= "x")
                |> List.map (Tuple.mapSecond (String.toInt >> Maybe.withDefault -1))
    in
    countT schedules 0


countT : List ( Delay, BusId ) -> Int -> Int
countT schedules t =
    let
        matches : List BusId
        matches =
            schedules
                |> List.filter (\( delay, id ) -> modBy id (t + delay) == 0)
                |> List.map Tuple.second
    in
    if List.length matches == List.length schedules then
        t

    else
        countT schedules (t + List.product matches)


type alias Schedule =
    { depart : Int
    , busIds : List Int
    }


part1 : String -> Int
part1 data =
    let
        schedule : Schedule
        schedule =
            parseSchedule data

        diffs : List ( Int, Int )
        diffs =
            List.map (countDiff schedule.depart) schedule.busIds

        min : ( Int, Maybe Int )
        min =
            List.foldl getMin ( 0, Nothing ) diffs
    in
    Tuple.first min * Maybe.withDefault -1 (Tuple.second min)


countDiff : Int -> Int -> ( Int, Int )
countDiff depart busId =
    let
        diff : Int
        diff =
            ceiling (toFloat depart / toFloat busId) * busId - depart
    in
    ( busId, diff )


getMin : ( Int, Int ) -> ( Int, Maybe Int ) -> ( Int, Maybe Int )
getMin ( id1, diff ) ( id2, maybe ) =
    case maybe of
        Just min ->
            if diff < min then
                ( id1, Just diff )

            else
                ( id2, Just min )

        Nothing ->
            ( id1, Just diff )


parseSchedule : String -> Schedule
parseSchedule data =
    let
        line : Parser String
        line =
            Parser.succeed ()
                |. Parser.chompUntilEndOr "\n"
                |> Parser.getChompedString

        parseListInt : String -> List Int
        parseListInt s =
            s
                |> String.split ","
                |> List.filter (\x -> x /= "x")
                |> List.filterMap String.toInt

        parser : Parser ( Int, List Int )
        parser =
            Parser.succeed (\x y -> ( x, parseListInt y ))
                |= Parser.int
                |. Parser.spaces
                |= line

        tuple : ( Int, List Int )
        tuple =
            Result.toMaybe (Parser.run parser data) |> Maybe.withDefault ( -1, [] )
    in
    Schedule (Tuple.first tuple) (Tuple.second tuple)
