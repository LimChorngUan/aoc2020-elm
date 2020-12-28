module Day9 exposing (part1, part2)

import Array exposing (Array)
import Debug


part1 : String -> Int
part1 data =
    let
        xs : Array Int
        xs =
            data
                |> String.lines
                |> List.filterMap String.toInt
                |> Array.fromList
    in
    helper1 xs 25


part2 : String -> Maybe Int
part2 data =
    let
        xs : Array Int
        xs =
            data
                |> String.lines
                |> List.filterMap String.toInt
                |> Array.fromList

        target : Int
        target =
            part1 data

        slice : List Int
        slice =
            helper2 xs target 0 1
    in
    case ( List.maximum slice, List.minimum slice ) of
        ( Just max, Just min ) ->
            Just (max + min)

        _ ->
            Nothing


helper1 : Array Int -> Int -> Int
helper1 xs p =
    let
        sums : List Int
        sums =
            xs
                |> Array.slice (p - 25) p
                |> Array.toList
                |> genPairCombs
                |> List.map (\e -> Tuple.first e + Tuple.second e)

        x : Int
        x =
            Array.get p xs |> Maybe.withDefault -1
    in
    if List.member x sums == False then
        x

    else
        helper1 xs (p + 1)


helper2 : Array Int -> Int -> Int -> Int -> List Int
helper2 arr target start end =
    let
        slice : List Int
        slice =
            arr
                |> Array.slice start (end + 1)
                |> Array.toList

        sum : Int
        sum =
            List.sum slice
    in
    if sum == target then
        slice

    else if sum < target then
        helper2 arr target start (end + 1)

    else
        helper2 arr target (start + 1) (start + 2)


genPairCombs : List Int -> List ( Int, Int )
genPairCombs xs =
    xs
        |> List.concatMap (\e1 -> List.map (\e2 -> ( e1, e2 )) xs)
        |> List.filter (\x -> Tuple.first x /= Tuple.second x)
