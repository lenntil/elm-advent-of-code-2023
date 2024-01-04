module Day5 exposing (solvePart1, solvePart2)


type alias SingleInst =
    ( ( Int, Int ), Int )


toInstruction : String -> List SingleInst
toInstruction para =
    case String.lines para of
        _ :: rest ->
            rest
                |> List.map
                    (\s ->
                        String.split " " s
                            |> List.filterMap String.toInt
                            |> (\ll ->
                                    case ll of
                                        [ to, from, len ] ->
                                            ( ( from, from + len ), to )

                                        _ ->
                                            ( ( 0, 0 ), 0 )
                                -- Debug.todo "Not recheable"
                               )
                    )
                |> List.sortBy (Tuple.first >> Tuple.first)

        _ ->
            []


solvePart1 : String -> String
solvePart1 multiline =
    let
        toSeeds : String -> List Int
        toSeeds str =
            case String.split " " str of
                "seeds:" :: rest ->
                    List.filterMap String.toInt rest

                _ ->
                    []

        toNext : List SingleInst -> Int -> Int
        toNext inst pos =
            case inst of
                [] ->
                    pos

                ( ( fromStart, fromEnd ), toStart ) :: rest ->
                    if pos >= fromStart && pos < fromEnd then
                        toStart
                            + (pos - fromStart)

                    else
                        toNext rest pos

        sendSeed : List (List SingleInst) -> Int -> Int
        sendSeed fullInst pos =
            case fullInst of
                [] ->
                    pos

                x :: rest ->
                    sendSeed rest (toNext x pos)
    in
    (case String.split "\n\n" multiline of
        seedPart :: instPart ->
            let
                seeds =
                    toSeeds seedPart
            in
            seeds
                |> List.map
                    (sendSeed
                        (instPart
                            |> List.map toInstruction
                        )
                    )
                |> List.minimum
                |> Maybe.withDefault 0

        _ ->
            0
    )
        |> String.fromInt

solvePart2 : String -> String
solvePart2 multiline = "0"
