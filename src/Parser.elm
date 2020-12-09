module Parser exposing (..)

import Array
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


parseNestedArrayString : String -> Array.Array (Array.Array String)
parseNestedArrayString s =
    s
        |> String.split "\n"
        |> List.map String.trim
        |> List.map (List.filter (not << String.isEmpty) << String.split "")
        |> List.filter (not << List.isEmpty)
        |> List.map Array.fromList
        |> Array.fromList
