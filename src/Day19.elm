module Day19 exposing (..)

import Dict exposing (Dict)
import Parser exposing (Parser, (|.), (|=))
import Array
import String exposing (String)
import Set


type alias EncryptedRules =
    Dict Int (List (List Int))


type alias DecodedRules =
    Dict Int (List String)


part1 : String -> Int
part1 data =
    let
        msgs : List String
        msgs = parseMsgs data

        rules : List String
        rules =
            decodeRules (parseDecodedRules data) (parseEncryptedRules data)
                |> Dict.get 0
                |> Maybe.withDefault []

    in
    msgs
        |> List.filter (\msg -> List.member msg rules)
        |> List.length

decodeRules : DecodedRules -> EncryptedRules -> DecodedRules
decodeRules decoded encrypted =
    if Dict.isEmpty encrypted then
        decoded
    else
    let
        newlyDecoded =
            encrypted
                |> Dict.filter (canBeDecoded decoded)
                |> Dict.map (decodeRule decoded)

        remainingEncrypted =
            Dict.keys newlyDecoded
                |> List.foldl (\k dict -> Dict.remove k dict) encrypted

    in
    decodeRules (Dict.union decoded newlyDecoded) remainingEncrypted


canBeDecoded : DecodedRules -> Int -> (List (List Int)) -> Bool
canBeDecoded decodedRules k v =
    v
        |> List.concat
        |> Set.fromList
        |> Set.toList
        |> List.all (\i -> Dict.member i decodedRules)


decodeRule : DecodedRules -> Int -> List (List Int) -> List String
decodeRule decodedRules k v =
    v
        |> List.map (List.map (\x -> Dict.get x decodedRules |> Maybe.withDefault []))
        |> List.concatMap getPairs


getPairs : List (List String) -> List String
getPairs lists =
    if List.length lists == 1 then
        List.head lists |> Maybe.withDefault []
    else
    let
        arr = Array.fromList lists

        xs = Array.get 0 arr |> Maybe.withDefault []
        ys = Array.get 1 arr |> Maybe.withDefault []

        res1 = xs |> List.concatMap (\x -> List.map (\y -> String.append x y) ys)
        res2 = xs |> List.concatMap (\x -> List.map (\y -> String.append x y) (List.reverse ys))
    in
    List.append res1 res2
        |> Set.fromList
        |> Set.toList


-- PARSER


parseMsgs : String -> List String
parseMsgs data =
    data
        |> String.split "\n\n"
        |> List.tail >> Maybe.withDefault []
        |> List.head >> Maybe.withDefault ""
        |> String.lines


parseDecodedRules : String -> DecodedRules
parseDecodedRules data =
    data
        |> String.split "\n\n"
        |> List.head >> Maybe.withDefault ""
        |> String.lines
        |> List.filterMap (Result.toMaybe << Parser.run decodedRuleParser)
        |> Dict.fromList


parseEncryptedRules : String -> EncryptedRules
parseEncryptedRules data =
    data
        |> String.split "\n\n"
        |> List.head >> Maybe.withDefault ""
        |> String.lines
        |> List.filterMap (Result.toMaybe << Parser.run encryptedRuleParser)
        |> Dict.fromList


decodedRuleParser : Parser (Int, List String)
decodedRuleParser =
    let
        kParser =
            Parser.succeed ()
                |. Parser.chompUntil ":"
                |> Parser.getChompedString

        vParser =
            Parser.succeed ()
                |. Parser.chompUntil "\""
                |> Parser.getChompedString
    in
    Parser.succeed (\k v -> (String.toInt k |> Maybe.withDefault -1, [v]))
        |= kParser
        |. Parser.symbol ":"
        |. Parser.spaces
        |. Parser.symbol "\""
        |= vParser
        |. Parser.symbol "\""


encryptedRuleParser : Parser (Int, List (List Int))
encryptedRuleParser =
    let
        kParser =
            Parser.succeed ()
              |. Parser.chompUntil ":"
                |> Parser.getChompedString

        vParser =
            Parser.succeed ()
                |. Parser.chompIf (\c -> c /= '"')
                |. Parser.chompWhile (\c -> c /= '"')
                |> Parser.getChompedString

        toRules s =
            s
                |> String.split "|"
                |> List.map (List.filterMap String.toInt << String.split " ")

    in
    Parser.succeed (\k v -> (String.toInt k |> Maybe.withDefault -1, toRules v))
        |= kParser
        |. Parser.symbol ":"
        |. Parser.spaces
        |= vParser