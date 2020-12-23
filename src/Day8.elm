module Day8 exposing (..)

import Array exposing (Array)
import Debug
import Html exposing (s)
import Parser exposing ((|.), (|=), Parser)


type Operation
    = Acc
    | Jmp
    | Nop


type Sign
    = Plus
    | Minus


type alias Command =
    { op : Maybe Operation
    , sign : Maybe Sign
    , arg : Int
    }


part2 : String -> Int
part2 data =
    let
        errCmds : List Command
        errCmds =
            parseCommands data

        tries : List (List Command)
        tries =
            genAllTries (Array.fromList errCmds) 0 []
    in
    tries
        |> List.map (runCommand 0 [] 0)
        |> List.filter (\e -> e /= -1)
        |> List.head
        |> Maybe.withDefault -1


part1 : String -> Int
part1 data =
    let
        cmds : List Command
        cmds =
            parseCommands data
    in
    runCommand 0 [] 0 cmds


runCommand : Int -> List Int -> Int -> List Command -> Int
runCommand i execIs acc cmds =
    if i == List.length cmds then
        acc

    else if List.member i execIs then
        -- Change this to acc to get solution for part1
        -1

    else
        let
            cmd : Maybe Command
            cmd =
                cmds
                    |> Array.fromList
                    |> Array.get i
        in
        case cmd of
            Just c ->
                case c.op of
                    Just Acc ->
                        case c.sign of
                            Just Plus ->
                                runCommand (i + 1) (i :: execIs) (acc + c.arg) cmds

                            Just Minus ->
                                runCommand (i + 1) (i :: execIs) (acc - c.arg) cmds

                            Nothing ->
                                -1

                    Just Jmp ->
                        case c.sign of
                            Just Plus ->
                                runCommand (i + c.arg) (i :: execIs) acc cmds

                            Just Minus ->
                                runCommand (i - c.arg) (i :: execIs) acc cmds

                            Nothing ->
                                -1

                    Just Nop ->
                        runCommand (i + 1) (i :: execIs) acc cmds

                    Nothing ->
                        -1

            Nothing ->
                -1


genAllTries : Array Command -> Int -> List (List Command) -> List (List Command)
genAllTries cmds i tries =
    if i == Array.length cmds then
        tries

    else
        case Array.get i cmds of
            Just cmd ->
                case cmd.op of
                    Just Jmp ->
                        let
                            fix : List Command
                            fix =
                                cmds
                                    |> Array.set i { cmd | op = Just Nop }
                                    |> Array.toList
                        in
                        genAllTries cmds (i + 1) (fix :: tries)

                    Just Nop ->
                        let
                            fix : List Command
                            fix =
                                cmds
                                    |> Array.set i { cmd | op = Just Jmp }
                                    |> Array.toList
                        in
                        genAllTries cmds (i + 1) (fix :: tries)

                    _ ->
                        genAllTries cmds (i + 1) tries

            Nothing ->
                []


parseCommands : String -> List Command
parseCommands data =
    let
        word : Parser String
        word =
            Parser.succeed ()
                |. Parser.chompUntil " "
                |> Parser.getChompedString

        sign : Parser String
        sign =
            Parser.succeed ()
                |. Parser.chompIf (\c -> c == '+' || c == '-')
                |> Parser.getChompedString

        parser : Parser Command
        parser =
            Parser.succeed (\x y z -> Command (parseOp x) (parseSign y) z)
                |= word
                |. Parser.spaces
                |= sign
                |= Parser.int
    in
    data
        |> String.lines
        |> List.filterMap (Result.toMaybe << Parser.run parser)
        |> List.filter (\e -> e.op /= Nothing && e.sign /= Nothing)


parseOp : String -> Maybe Operation
parseOp x =
    case x of
        "acc" ->
            Just Acc

        "jmp" ->
            Just Jmp

        "nop" ->
            Just Nop

        _ ->
            Nothing


parseSign : String -> Maybe Sign
parseSign x =
    case x of
        "+" ->
            Just Plus

        "-" ->
            Just Minus

        _ ->
            Nothing
