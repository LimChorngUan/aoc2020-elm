module Day6 exposing (count1, count2)

import Set
import String exposing (String)


count1 : String -> Int
count1 data =
    data
        |> String.replace "\n" " "
        |> String.split "  "
        |> List.map (Set.fromList << String.split "" << String.replace " " "")
        |> List.foldl ((+) << Set.size) 0


count2 : String -> Int
count2 data =
    let
        union : Set.Set String
        union =
            data
                |> String.replace "\n" " "
                |> String.split "  "
                |> List.map (Set.fromList << String.split "" << String.replace " " "")
                |> List.foldl Set.union (Set.fromList [])
    in
    data
        |> String.replace "\n" " "
        |> String.split "  "
        |> List.map (String.split " " >> List.map (String.split "" >> Set.fromList))
        |> List.map (List.foldl Set.intersect union)
        |> List.foldl ((+) << Set.size) 0
