module Day10 exposing (part1)


type alias DiffCounter =
    { prev : Maybe Int
    , one : Int
    , two : Int
    , three : Int
    , slicePos : List Int
    }


part1 : String -> Int
part1 data =
    let
        diffCounter : DiffCounter
        diffCounter =
            parseDiffCounter data
    in
    diffCounter.one * (diffCounter.three + 1)


countDiff : ( Int, Int ) -> DiffCounter -> DiffCounter
countDiff ( i, x ) counter =
    case counter.prev of
        Nothing ->
            { counter | prev = Just x }

        Just prev ->
            case x - prev of
                1 ->
                    { counter | prev = Just x, one = counter.one + 1 }

                2 ->
                    { counter | prev = Just x, two = counter.two + 1 }

                3 ->
                    { counter | prev = Just x, three = counter.three + 1, slicePos = i :: counter.slicePos }

                _ ->
                    counter


parseDiffCounter : String -> DiffCounter
parseDiffCounter data =
    data
        |> String.lines
        |> List.filterMap String.toInt
        |> List.sort
        |> (::) 0
        |> List.indexedMap Tuple.pair
        |> List.foldl countDiff (DiffCounter Nothing 0 0 0 [])
