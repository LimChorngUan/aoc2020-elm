module Day2 exposing (..)

import Maybe
import Parser exposing ((|.), (|=))


type alias PassEntry =
    { password : String
    , char : String
    , first : Int
    , second : Int
    }


validatePassword1 : String -> Int
validatePassword1 data =
    data
        |> parseListStr
        |> List.map genPassword
        |> List.filter isValidPassword1
        |> List.length


validatePassword2 : String -> Int
validatePassword2 data =
    data
        |> parseListStr
        |> List.map genPassword
        |> List.filter isValidPassword2
        |> List.length


genPassword : String -> Maybe PassEntry
genPassword x =
    case String.words x of
        [ minMax, char, pw ] ->
            case String.split "-" minMax of
                [ first, second ] ->
                    Just <|
                        PassEntry
                            pw
                            (String.slice 0 1 char)
                            (String.toInt first |> Maybe.withDefault 0)
                            (String.toInt second |> Maybe.withDefault 0)

                _ ->
                    Nothing

        _ ->
            Nothing


isValidPassword1 : Maybe PassEntry -> Bool
isValidPassword1 x =
    case x of
        Just passEntry ->
            let
                amountMatch : Int
                amountMatch =
                    passEntry.password
                        |> String.split ""
                        |> List.filter (\c -> c == passEntry.char)
                        |> List.length
            in
            amountMatch >= passEntry.first && amountMatch <= passEntry.second

        Nothing ->
            False


isValidPassword2 : Maybe PassEntry -> Bool
isValidPassword2 x =
    case x of
        Just passEntry ->
            let
                matches : List String
                matches =
                    passEntry.password
                        |> String.split ""
                        |> List.indexedMap
                            (\i s ->
                                if i == passEntry.first - 1 || i == passEntry.second - 1 then
                                    Just s

                                else
                                    Nothing
                            )
                        |> List.filterMap identity
                        |> List.filter (\c -> c == passEntry.char)
            in
            List.length matches == 1

        Nothing ->
            False


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
