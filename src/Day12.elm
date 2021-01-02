module Day12 exposing (ferryGoPuPuuuu)

import Array exposing (Array)
import Parser exposing ((|.), (|=), Parser)


type Cmd
    = Move Direction Int
    | Rotate Rotation Int
    | Forward Int


type Direction
    = N
    | S
    | E
    | W


type Rotation
    = R
    | L


type alias Coordinate =
    { waypoint : ( Int, Int )
    , position : ( Int, Int )
    }


ferryGoPuPuuuu : String -> Int
ferryGoPuPuuuu data =
    let
        cmds : List Cmd
        cmds =
            parseCmds data

        init : Coordinate
        init =
            Coordinate ( 10, 1 ) ( 0, 0 )

        final : Coordinate
        final =
            List.foldl executeCmd init cmds
    in
    abs (Tuple.first final.position) + abs (Tuple.second final.position)


executeCmd : Cmd -> Coordinate -> Coordinate
executeCmd cmd coordinate =
    case cmd of
        Move dir v ->
            move dir v coordinate

        Rotate rot v ->
            rotate rot (v // 90) coordinate

        Forward v ->
            forward v coordinate


move : Direction -> Int -> Coordinate -> Coordinate
move dir v coordinate =
    let
        x =
            Tuple.first coordinate.waypoint

        y =
            Tuple.second coordinate.waypoint
    in
    case dir of
        N ->
            { coordinate | waypoint = ( x, y + v ) }

        S ->
            { coordinate | waypoint = ( x, y - v ) }

        E ->
            { coordinate | waypoint = ( x + v, y ) }

        W ->
            { coordinate | waypoint = ( x - v, y ) }


forward : Int -> Coordinate -> Coordinate
forward v coordinate =
    let
        waypointX =
            Tuple.first coordinate.waypoint

        waypointY =
            Tuple.second coordinate.waypoint

        posX =
            Tuple.first coordinate.position

        posY =
            Tuple.second coordinate.position
    in
    { coordinate | position = ( posX + waypointX * v, posY + waypointY * v ) }


rotate : Rotation -> Int -> Coordinate -> Coordinate
rotate rotation n coordinate =
    if n == 0 then
        coordinate

    else
        let
            x =
                Tuple.first coordinate.waypoint

            y =
                Tuple.second coordinate.waypoint
        in
        case rotation of
            R ->
                rotate R (n - 1) { coordinate | waypoint = ( y, negate x ) }

            L ->
                rotate L (n - 1) { coordinate | waypoint = ( negate y, x ) }


parseCmds : String -> List Cmd
parseCmds data =
    let
        uppercaseParser : Parser String
        uppercaseParser =
            Parser.succeed ()
                |. Parser.chompIf Char.isUpper
                |> Parser.getChompedString

        keyValueParser : Parser ( String, Int )
        keyValueParser =
            Parser.succeed Tuple.pair
                |= uppercaseParser
                |= Parser.int

        parseCmd : ( String, Int ) -> Maybe Cmd
        parseCmd ( k, v ) =
            case k of
                "N" ->
                    Just (Move N v)

                "S" ->
                    Just (Move S v)

                "E" ->
                    Just (Move E v)

                "W" ->
                    Just (Move W v)

                "R" ->
                    Just (Rotate R v)

                "L" ->
                    Just (Rotate L v)

                "F" ->
                    Just (Forward v)

                _ ->
                    Nothing
    in
    data
        |> String.lines
        |> List.filterMap (Result.toMaybe << Parser.run keyValueParser)
        |> List.filterMap parseCmd


elemIndex : a -> Array a -> Int
elemIndex target xs =
    xs
        |> Array.toIndexedList
        |> List.filter (\( i, x ) -> x == target)
        |> List.head
        |> Maybe.withDefault ( -1, target )
        |> Tuple.first
