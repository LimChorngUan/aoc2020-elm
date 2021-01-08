module Day16 exposing (part1)

import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)
import Dict exposing (Dict)


type alias Notes =
    { rules : Dict String (List Int)
    , myTicket : List Int
    , nearbyTickets : List (List Int)
    }


part1 : String -> Int
part1 data =
    let
        notes = parseNotes data

        allValidRange : List Int
        allValidRange =
            notes.rules
                |> Dict.values
                |> List.concat
                |> Set.fromList
                |> Set.toList
    in
    notes.nearbyTickets
        |> List.concat
        |> List.filter (\e -> List.member e allValidRange |> not)
        |> List.sum


parseNotes : String -> Notes
parseNotes data =
    case String.split "\n\n" data of
        [x, y, z] ->
            let
                rules =
                    x
                        |> String.lines
                        |> List.filterMap parseRule
                        |> Dict.fromList

                myTicket =
                    y
                        |> String.lines
                        |> List.drop 1
                        |> List.concatMap (String.split ",")
                        |> List.filterMap String.toInt

                nearbyTickets =
                    z
                        |> String.lines
                        |> List.map (String.split ",")
                        |> List.map (List.filterMap String.toInt)
                        |> List.filter (List.isEmpty >> not)

            in
            Notes rules myTicket nearbyTickets

        _ ->
            Notes Dict.empty [] [[]]


parseRule : String -> Maybe (String, List Int)
parseRule x =
    let
        wordParser : Parser String
        wordParser =
            Parser.succeed ()
                |. Parser.chompUntil ": "
                |> Parser.getChompedString

        ruleParser : Parser (String, List Int)
        ruleParser =
            Parser.succeed (\s x1 x2 y1 y2 -> (s, List.append (List.range x1 x2) (List.range y1 y2)))
                |= wordParser
                |. Parser.symbol ":"
                |. Parser.spaces
                |= Parser.int
                |. Parser.symbol "-"
                |= Parser.int
                |. Parser.spaces
                |. Parser.keyword "or"
                |. Parser.spaces
                |= Parser.int
                |. Parser.symbol "-"
                |= Parser.int

    in
    x
        |> Parser.run ruleParser
        |> Result.toMaybe