module Day14 exposing (countSum)

import Array exposing (Array)
import Binary
import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser)


type alias Program =
    { mask : List MaskBit
    , memory : Dict Memory Value
    }


type Cmd
    = UpdateMask (List MaskBit)
    | WriteValue Memory Value


type MaskBit
    = Zero
    | One
    | X


type alias Memory =
    Int


type alias Value =
    Int


countSum : String -> Int
countSum data =
    let
        cmds : List Cmd
        cmds =
            parseCmds data

        init : Program
        init =
            Program (List.repeat 36 X) Dict.empty

        res : Program
        res =
            List.foldl execCmd init cmds
    in
    res.memory
        |> Dict.values
        |> List.sum


execCmd : Cmd -> Program -> Program
execCmd cmd prog =
    case cmd of
        UpdateMask mask ->
            { prog | mask = mask }

        WriteValue memory value ->
            writeValue memory value prog



-- Part 1


writeValue : Memory -> Value -> Program -> Program
writeValue memory value prog =
    let
        bits : List Int
        bits =
            value
                |> Binary.fromDecimal
                |> Binary.ensureSize 36
                |> Binary.toIntegers

        overwriteBit : MaskBit -> Int -> Int
        overwriteBit maskBit valueBit =
            case maskBit of
                Zero ->
                    0

                One ->
                    1

                X ->
                    valueBit

        result : Value
        result =
            List.map2 overwriteBit prog.mask bits
                |> Binary.fromIntegers
                |> Binary.toDecimal
    in
    { prog | memory = Dict.insert memory result prog.memory }



-- Part 2
-- writeValue : Memory -> Value -> Program -> Program
-- writeValue memory value prog =
--     let
--         overwrite : MaskBit -> Int -> MaskBit
--         overwrite maskBit memoryBit =
--             case maskBit of
--                 Zero ->
--                     if memoryBit == 0 then
--                         Zero
--                     else
--                         One
--                 One ->
--                     One
--                 X ->
--                     X
--         addresses : List Memory
--         addresses =
--             getAddresses prog.mask memory
--     in
--     { prog | memory = List.foldl (\add mem -> Dict.insert add value mem) prog.memory addresses }


getAddresses : List MaskBit -> Int -> List Int
getAddresses mask memory =
    let
        bits =
            memory
                |> Binary.fromDecimal
                |> Binary.ensureSize (List.length mask)
                |> Binary.toIntegers
    in
    List.map2
        (\b m ->
            case m of
                Zero ->
                    if b == 0 then
                        Zero

                    else
                        One

                One ->
                    One

                X ->
                    X
        )
        bits
        mask
        |> List.indexedMap Tuple.pair
        |> List.foldl
            (\( i, b ) all ->
                case b of
                    Zero ->
                        all |> List.map (Array.set i 0)

                    One ->
                        all |> List.map (Array.set i 1)

                    X ->
                        all
                            |> List.map
                                (\a ->
                                    [ Array.set i 0 a, Array.set i 1 a ]
                                )
                            |> List.concat
            )
            [ List.repeat 36 0 |> Array.fromList ]
        |> List.map
            (Array.toList
                >> Binary.fromIntegers
                >> Binary.toDecimal
            )


parseCmds : String -> List Cmd
parseCmds data =
    let
        parseMask : String -> List MaskBit
        parseMask s =
            let
                parseMaskBit : String -> MaskBit
                parseMaskBit b =
                    case b of
                        "0" ->
                            Zero

                        "1" ->
                            One

                        "X" ->
                            X

                        _ ->
                            X
            in
            s
                |> String.dropLeft 7
                |> String.split ""
                |> List.map parseMaskBit

        memoryParser : Parser ( Memory, Value )
        memoryParser =
            Parser.succeed (\m v -> ( m, v ))
                |. Parser.keyword "mem"
                |. Parser.symbol "["
                |= Parser.int
                |. Parser.symbol "]"
                |. Parser.spaces
                |. Parser.symbol "="
                |. Parser.spaces
                |= Parser.int

        parseCmd : String -> Maybe Cmd
        parseCmd s =
            case String.left 3 s of
                "mas" ->
                    Just (UpdateMask (parseMask s))

                "mem" ->
                    let
                        memoryValuePair : ( Memory, Value )
                        memoryValuePair =
                            Maybe.withDefault ( -1, -1 ) (Result.toMaybe (Parser.run memoryParser s))

                        memory : Memory
                        memory =
                            Tuple.first memoryValuePair

                        value : Value
                        value =
                            Tuple.second memoryValuePair
                    in
                    Just (WriteValue memory value)

                _ ->
                    Nothing
    in
    data
        |> String.lines
        |> List.filterMap parseCmd
