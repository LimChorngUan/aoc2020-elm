module Day17 exposing (getActiveCubes)

import Dict.Any as Dict exposing (AnyDict)
import Set.Any as Set


type alias Point =
    { x : Int
    , y : Int
    , z : Int
    , w : Int
    }


type State
    = Active
    | Inactive


getActiveCubes : String -> Int
getActiveCubes data =
    runSimulation 0 (parsePointState data)
        |> Dict.values
        |> List.filter (\state -> state == Active)
        |> List.length


runSimulation : Int -> AnyDict String Point State -> AnyDict String Point State
runSimulation n init =
    if n == 6 then
        init

    else
        let
            updated =
                List.foldl (updateState init) (Dict.empty keyString) (getAllPointsToUpdate init)
        in
        runSimulation (n + 1) updated


updateState : AnyDict String Point State -> Point -> AnyDict String Point State -> AnyDict String Point State
updateState init point dict =
    let
        initState =
            init
                |> Dict.get point
                |> Maybe.withDefault Inactive

        amountNeighbourActiveState =
            point
                |> getNeighbours
                |> List.filterMap (\p -> Dict.get p init)
                |> List.filter (\state -> state == Active)
                |> List.length
    in
    case initState of
        Active ->
            if amountNeighbourActiveState /= 2 && amountNeighbourActiveState /= 3 then
                Dict.insert point Inactive dict

            else
                Dict.insert point Active dict

        Inactive ->
            if amountNeighbourActiveState == 3 then
                Dict.insert point Active dict

            else
                Dict.insert point Inactive dict


getAllPointsToUpdate : AnyDict String Point State -> List Point
getAllPointsToUpdate dict =
    let
        neighbours =
            dict
                |> Dict.keys
                |> List.concatMap getNeighbours
                |> Set.fromList keyString
    in
    dict
        |> Dict.keys
        |> Set.fromList keyString
        |> Set.union neighbours
        |> Set.toList


getNeighbours : Point -> List Point
getNeighbours point =
    List.range -1 1
        |> List.concatMap
            (\dx ->
                List.range -1 1
                    |> List.concatMap
                        (\dy ->
                            List.range -1 1
                                |> List.concatMap
                                    (\dz ->
                                        List.range -1 1
                                            |> List.map (\dw -> Point (point.x + dx) (point.y + dy) (point.z + dz) (point.w + dw))
                                    )
                        )
            )
        |> List.filter (\p -> p /= point)


parsePointState : String -> AnyDict String Point State
parsePointState data =
    let
        parseState : String -> State
        parseState s =
            case s of
                "#" ->
                    Active

                "." ->
                    Inactive

                _ ->
                    Inactive
    in
    data
        |> String.lines
        |> List.map (String.split "")
        -- intial z and w is 0
        |> List.indexedMap (\x row -> List.indexedMap (\y col -> ( Point x y 0 0, parseState col )) row)
        |> List.concat
        |> Dict.fromList keyString


keyString : Point -> String
keyString point =
    [ point.x, point.y, point.z, point.w ]
        |> List.map String.fromInt
        |> String.join " "
