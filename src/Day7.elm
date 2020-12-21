module Day7 exposing (part1)

import Debug
import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser)


type alias Content =
    { count : Int
    , bag : String
    }


part1 : String -> Int
part1 data =
    let
        rules : Dict String (List Content)
        rules =
            getRules data

        keys =
            Dict.keys rules
    in
    keys
        |> List.map (help1 rules)
        |> List.filter (\e -> e == True)
        |> List.length


help1 : Dict String (List Content) -> String -> Bool
help1 dict bag =
    if bag == "shiny gold" then
        False

    else
        case Dict.get bag dict of
            Just contents ->
                if List.any (\content -> content.bag == "shiny gold") contents then
                    True

                else
                    let
                        xs : List Bool
                        xs =
                            List.map (\content -> help1 dict content.bag) contents
                    in
                    if List.any (\x -> x == True) xs then
                        True

                    else
                        False

            Nothing ->
                False


getRules : String -> Dict String (List Content)
getRules data =
    data
        |> String.lines
        |> List.filterMap parseRule
        |> Dict.fromList


parseRule : String -> Maybe ( String, List Content )
parseRule line =
    let
        xs : List String
        xs =
            String.split "contain " line
    in
    case xs of
        [ bagStr, contentsStr ] ->
            let
                bag =
                    Maybe.withDefault "" (Result.toMaybe (Parser.run bagParser bagStr))

                contents =
                    contentsStr
                        |> String.split ", "
                        |> List.filterMap (Result.toMaybe << Parser.run contentParser)
            in
            Just ( bag, contents )

        _ ->
            Nothing


bagParser : Parser String
bagParser =
    Parser.succeed ()
        |. Parser.oneOf
            [ Parser.chompUntil " bag"
            , Parser.chompUntil " bags"
            ]
        |> Parser.getChompedString


contentParser : Parser Content
contentParser =
    Parser.succeed Content
        |= Parser.int
        |. Parser.spaces
        |= bagParser
