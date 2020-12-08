module Parser exposing (..)

import Maybe


parseListInt : String -> List Int
parseListInt s =
    s
        |> String.split "\n"
        |> List.map String.trim
        |> List.filterMap String.toInt


parseListStr : String -> List String
parseListStr s =
    s
        |> String.split "\n"
        |> List.map String.trim
        |> List.filter (not << String.isEmpty)
