module Day1 exposing (part1, part2)

import Maybe
import Parser


part1 : String -> Maybe Int
part1 data =
    data
        |> parseListInt
        |> genPairCombs
        |> List.filter sumIs2020
        |> List.head
        |> getProduct


part2 : String -> Maybe Int
part2 data =
    data
        |> parseListInt
        |> genTripleCombs
        |> List.filter sumIs2020
        |> List.head
        |> getProduct


genPairCombs : List a -> List (List a)
genPairCombs xs =
    List.concatMap (\e1 -> List.map (\e2 -> [ e1, e2 ]) xs) xs



-- Nah this is too slow :(


genTripleCombs : List a -> List (List a)
genTripleCombs xs =
    List.concatMap (\e1 -> List.concatMap (\e2 -> List.map (\e3 -> [ e1, e2, e3 ]) xs) xs) xs


sumIs2020 : List Int -> Bool
sumIs2020 xs =
    List.sum xs == 2020


getProduct : Maybe (List Int) -> Maybe Int
getProduct maybe =
    case maybe of
        Just xs ->
            Just (List.product xs)

        Nothing ->
            Nothing


listIntParser : Parser.Parser (List Int)
listIntParser =
    Parser.sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = Parser.spaces
        , item = Parser.int
        , trailing = Parser.Optional
        }


parseListInt : String -> List Int
parseListInt data =
    data
        |> Parser.run listIntParser
        |> Result.toMaybe
        |> Maybe.withDefault []
