module Day19 exposing (..)

import Dict exposing (Dict)
import Parser exposing (Parser, (|.), (|=))
import Array
import String exposing (String)
import Set


type alias EncryptedRules =
    Dict Int (List (List Int))


type alias DecodedRules =
    Dict Int (List (List String))


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

        f = Debug.log "newlyDecoded" newlyDecoded
    in
    decoded


canBeDecoded : DecodedRules -> Int -> (List (List Int)) -> Bool
canBeDecoded decodedRules k v =
    v
        |> List.concat
        |> Set.fromList
        |> Set.toList
        |> List.all (\i -> Dict.member i decodedRules)


decodeRule : DecodedRules -> Int -> List (List Int) -> List (List String)
decodeRule decodedRules k v =
    v
        |> List.map (List.concatMap (\x -> Dict.get x decodedRules |> Maybe.withDefault [[]] |> List.concat))


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


decodedRuleParser : Parser (Int, List (List String))
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
    Parser.succeed (\k v -> (String.toInt k |> Maybe.withDefault -1, [[v]]))
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
                |> String.replace " " ""
                |> String.split "|"
                |> List.map (List.filterMap String.toInt << String.split "")

    in
    Parser.succeed (\k v -> (String.toInt k |> Maybe.withDefault -1, toRules v))
        |= kParser
        |. Parser.symbol ":"
        |. Parser.spaces
        |= vParser


test : String
test = """0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b"

ababbb
bababa
abbbab
aaabbb
aaaabbb"""