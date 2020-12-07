module Parser exposing (..)
import Maybe


parseListInt : String -> List Int
parseListInt s = s
    |> String.split "\n"
    |> List.map String.trim
    |> List.filterMap String.toInt
