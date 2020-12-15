module Day4 exposing (countValidPassport)

import Dict exposing (Dict)


type alias Passport =
    { byr : Maybe Int
    , iyr : Maybe Int
    , eyr : Maybe Int
    , hgt : Maybe Height
    , hcl : Maybe String
    , ecl : Maybe String
    , pid : Maybe String
    }


type alias Height =
    ( Int, HeightUnit )


type HeightUnit
    = Metric
    | Imperial


countValidPassport : String -> Int
countValidPassport data =
    data
        |> String.replace "\n" " "
        |> String.split "  "
        |> List.map parsePassport
        |> List.filter isValidPassport
        |> List.length


parsePassport : String -> Passport
parsePassport s =
    let
        dict : Dict.Dict String String
        dict =
            s
                |> String.split " "
                |> List.filterMap (listToTuple << String.split ":")
                |> Dict.fromList
    in
    Passport
        (Dict.get "byr" dict |> Maybe.andThen parseByr)
        (Dict.get "iyr" dict |> Maybe.andThen parseIyr)
        (Dict.get "eyr" dict |> Maybe.andThen parseEyr)
        (Dict.get "hgt" dict |> Maybe.andThen parseHgt)
        (Dict.get "hcl" dict |> Maybe.andThen parseHcl)
        (Dict.get "ecl" dict |> Maybe.andThen parseEcl)
        (Dict.get "pid" dict |> Maybe.andThen parsePid)


isValidPassport : Passport -> Bool
isValidPassport p =
    p.byr
        /= Nothing
        && p.iyr
        /= Nothing
        && p.eyr
        /= Nothing
        && p.hgt
        /= Nothing
        && p.hcl
        /= Nothing
        && p.ecl
        /= Nothing
        && p.pid
        /= Nothing


parseByr : String -> Maybe Int
parseByr s =
    case String.toInt s of
        Just year ->
            returnValueIfIsInRange 1920 2002 year

        Nothing ->
            Nothing


parseIyr : String -> Maybe Int
parseIyr s =
    case String.toInt s of
        Just year ->
            returnValueIfIsInRange 2010 2020 year

        Nothing ->
            Nothing


parseEyr : String -> Maybe Int
parseEyr s =
    case String.toInt s of
        Just year ->
            returnValueIfIsInRange 2020 2030 year

        Nothing ->
            Nothing



--  I am 149cm meaning so... I can't take the flight :(((


parseHgt : String -> Maybe Height
parseHgt s =
    let
        unit : Maybe HeightUnit
        unit =
            case String.right 2 s of
                "cm" ->
                    Just Metric

                "in" ->
                    Just Imperial

                _ ->
                    Maybe.Nothing

        value : Maybe Int
        value =
            case unit of
                Just Metric ->
                    s |> String.dropRight 2 |> String.toInt |> Maybe.andThen (returnValueIfIsInRange 150 193)

                Just Imperial ->
                    s |> String.dropRight 2 |> String.toInt |> Maybe.andThen (returnValueIfIsInRange 59 76)

                Nothing ->
                    Nothing
    in
    case ( value, unit ) of
        ( Just x, Just u ) ->
            Just ( x, u )

        _ ->
            Nothing


parseHcl : String -> Maybe String
parseHcl s =
    let
        hash =
            s |> String.left 1

        hex =
            s |> String.dropLeft 1
    in
    if hash == "#" && String.length hex == 6 && String.all Char.isHexDigit hex == True then
        Just s

    else
        Nothing


parseEcl : String -> Maybe String
parseEcl s =
    let
        enums =
            [ "amb", "blu", "brn", "gry", "grn", "hzl", "oth" ]
    in
    if List.member s enums then
        Just s

    else
        Nothing


parsePid : String -> Maybe String
parsePid s =
    if String.length s == 9 && String.all Char.isDigit s then
        Just s

    else
        Nothing


returnValueIfIsInRange : Int -> Int -> Int -> Maybe Int
returnValueIfIsInRange min max x =
    if x >= min && x <= max then
        Just x

    else
        Nothing


listToTuple : List a -> Maybe ( a, a )
listToTuple list =
    case list of
        [ a, b ] ->
            Just ( a, b )

        _ ->
            Nothing
