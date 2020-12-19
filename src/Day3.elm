module Day3 exposing (countTrees1, countTrees2)

import Array exposing (Array, get, length, map)
import Debug exposing (log)
import Html exposing (datalist)


tree : String
tree =
    "#"


walk : Int -> Int -> Int -> Int -> Array (Array String) -> Int -> Int
walk xStep yStep x y map trees =
    let
        xNew =
            x + xStep

        yNew =
            y + yStep

        row : Maybe (Array String)
        row =
            get yNew map
    in
    case row of
        Just xs ->
            let
                col : Maybe String
                col =
                    get (modBy (length xs) xNew) xs
            in
            case col of
                Just spot ->
                    if spot == tree then
                        walk xStep yStep xNew yNew map (trees + 1)

                    else
                        walk xStep yStep xNew yNew map trees

                Nothing ->
                    log "this shouldn't happen" 0

        Nothing ->
            trees


countTrees1 : String -> Int
countTrees1 data =
    let
        map : Array (Array String)
        map =
            parse data
    in
    walk 3 1 0 0 map 0


countTrees2 : String -> Int
countTrees2 data =
    let
        map : Array (Array String)
        map =
            parse data

        trees1 =
            walk 1 1 0 0 map 0

        trees2 =
            walk 3 1 0 0 map 0

        trees3 =
            walk 5 1 0 0 map 0

        trees4 =
            walk 7 1 0 0 map 0

        trees5 =
            walk 1 2 0 0 map 0
    in
    trees1 * trees2 * trees3 * trees4 * trees5


parse : String -> Array.Array (Array.Array String)
parse data =
    data
        |> String.lines
        |> List.map String.trim
        |> List.map (List.filter (not << String.isEmpty) << String.split "")
        |> List.filter (not << List.isEmpty)
        |> List.map Array.fromList
        |> Array.fromList
