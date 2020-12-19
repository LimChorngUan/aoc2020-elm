module Day5 exposing (findHighestSeatId, findMySeatId)

import Array exposing (Array)
import List exposing (head)
import Maybe exposing (withDefault)
import Parser exposing ((|.), (|=))


type Partition
    = Low
    | High


findHighestSeatId : String -> Maybe Int
findHighestSeatId data =
    data
        |> parseListStr
        |> List.map getSeatId
        |> List.maximum


findMySeatId : String -> Maybe Int
findMySeatId data =
    let
        ids : List Int
        ids =
            data
                |> parseListStr
                |> List.map getSeatId

        maxId =
            List.maximum ids

        minId =
            List.minimum ids
    in
    case ( minId, maxId ) of
        ( Just min, Just max ) ->
            Just ((List.range min max |> List.sum) - List.sum ids)

        _ ->
            Nothing


getSeatId : String -> Int
getSeatId line =
    let
        seq : List Partition
        seq =
            line
                |> String.split ""
                |> List.filterMap parsePartition

        ySeq =
            List.take 7 seq

        xSeq =
            List.drop 7 seq

        y =
            partition ( 0, 127 ) ySeq

        x =
            partition ( 0, 7 ) xSeq
    in
    y * 8 + x


partition : ( Int, Int ) -> List Partition -> Int
partition ( min, max ) seq =
    case seq of
        x :: [] ->
            case x of
                Low ->
                    min

                High ->
                    max

        x :: xs ->
            let
                upperMid : Int
                upperMid =
                    (min + max + 1) // 2
            in
            case x of
                Low ->
                    partition ( min, upperMid - 1 ) xs

                High ->
                    partition ( upperMid, max ) xs

        _ ->
            -1


parsePartition : String -> Maybe Partition
parsePartition s =
    case s of
        "F" ->
            Just Low

        "B" ->
            Just High

        "L" ->
            Just Low

        "R" ->
            Just High

        _ ->
            Nothing


parseListStr : String -> List String
parseListStr data =
    data
        |> Parser.run listStrParser
        |> Result.toMaybe
        |> Maybe.withDefault []


listStrParser : Parser.Parser (List String)
listStrParser =
    Parser.sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = Parser.spaces
        , item =
            Parser.getChompedString <|
                Parser.succeed ()
                    |. Parser.chompIf (\c -> c /= '\n')
                    |. Parser.chompWhile (\c -> c /= '\n')
        , trailing = Parser.Optional
        }
