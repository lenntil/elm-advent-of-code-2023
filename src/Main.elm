module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (onInput)
import Set exposing (Set)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



-- init


type alias Model =
    { input1 : String
    , input2 : String
    , input3 : String
    , input4 : String
    , input5 : String
    , input6 : String
    }


init : Model
init =
    { input1 = ""
    , input2 = ""
    , input3 = ""
    , input4 = ""
    , input5 = ""
    , input6 = ""
    }


type Msg
    = TextChanged Day String


type Day
    = Day1
    | Day2
    | Day3
    | Day4
    | Day5
    | Day6


type Problem
    = Problem1
    | Problem2
    | Problem3
    | Problem4
    | Problem5
    | Problem6
    | Problem7
    | Problem8



-- update


update : Msg -> Model -> Model
update msg model =
    case msg of
        TextChanged day input ->
            case day of
                Day1 ->
                    { model | input1 = input }

                Day2 ->
                    { model | input2 = input }

                Day3 ->
                    { model | input3 = input }

                Day4 ->
                    { model | input4 = input }

                Day5 ->
                    { model | input5 = input }

                Day6 ->
                    { model | input6 = input }


parseToInt : String -> Int
parseToInt input =
    String.trim input
        |> String.lines
        |> List.filter (String.isEmpty >> not)
        |> List.filterMap processLine
        |> List.foldl (+) 0


processLine : String -> Maybe Int
processLine line =
    String.trim line
        |> String.filter Char.isDigit
        |> addFirstLastText ""
        |> String.toInt


addFirstLastText : String -> String -> String
addFirstLastText middleText replaceText =
    String.left 1 replaceText ++ middleText ++ String.right 1 replaceText


alphabetsToNumber : List ( String, String )
alphabetsToNumber =
    [ ( "one", "1" ), ( "two", "2" ), ( "three", "3" ), ( "four", "4" ), ( "five", "5" ), ( "six", "6" ), ( "seven", "7" ), ( "eight", "8" ), ( "nine", "9" ) ]


substitute : List ( String, String ) -> String -> String
substitute substitutionTable text =
    substitutionTable
        |> List.map
            (\( first, second ) ->
                addFirstLastText second first
                    |> String.replace first
            )
        |> List.foldl (<|) text


preParse : List ( String, String ) -> String -> String
preParse substitutionTable text =
    String.trim text
        |> substitute substitutionTable


solve3 : String -> Int
solve3 multiline =
    String.trim multiline
        |> String.lines
        |> List.map
            (\line ->
                line
                    |> transform
                    |> String.split ", "
                    |> transform2
                    |> List.all ((==) True)
            )
        |> List.indexedMap Tuple.pair
        |> List.filter (\( _, e ) -> e == True)
        |> List.map (\( i, _ ) -> i + 1)
        |> List.sum


solve4 : String -> Int
solve4 multiline =
    String.trim multiline
        |> String.lines
        |> List.map
            (\line ->
                line
                    |> transform
                    |> String.split ", "
                    |> transform4
            )
        |> List.sum


transform : String -> String
transform line =
    line
        |> String.replace ":" "        "
        |> String.dropLeft 10
        |> String.trim
        |> String.replace ";" ","


transform2 : List String -> List Bool
transform2 words =
    [ ( "red", 12 ), ( "green", 13 ), ( "blue", 14 ) ]
        |> List.map
            (\( color, maxColor ) ->
                words
                    |> List.filter (String.contains color)
                    |> List.map (String.filter Char.isDigit)
                    |> List.filterMap String.toInt
                    |> List.all ((>=) maxColor)
            )


transform4 : List String -> Int
transform4 words =
    [ "red", "green", "blue" ]
        |> List.map
            (\color ->
                words
                    |> List.filter (String.contains color)
                    |> List.map (String.filter Char.isDigit)
                    |> List.filterMap String.toInt
                    |> List.maximum
                    |> Maybe.withDefault 1
            )
        |> List.foldl (*) 1



-- view


toStatusMessage : String -> String
toStatusMessage solution =
    case solution of
        "" ->
            "Error: Invalid input!"

        a ->
            "Result: " ++ a


viewSolution : Problem -> String -> Html Msg
viewSolution problem input =
    text <|
        case input of
            "" ->
                "Waiting for input..."

            a ->
                a
                    |> (case problem of
                            Problem1 ->
                                parseToInt

                            Problem2 ->
                                preParse alphabetsToNumber
                                    >> parseToInt

                            Problem3 ->
                                solve3

                            Problem4 ->
                                solve4

                            Problem5 ->
                                solve5

                            Problem6 ->
                                solve6

                            Problem7 ->
                                Debug.todo

                            Problem8 ->
                                Debug.todo
                       )
                    |> Debug.toString
                    |> toStatusMessage



-- Problem 5


toDottedLines : Set String -> String -> List ( Int, String )
toDottedLines symbols text =
    symbols
        |> Set.foldl (\s -> String.replace s ".") text
        |> String.lines
        |> List.indexedMap Tuple.pair


getDigitIndices : String -> List ( Int, Int )
getDigitIndices line =
    ("^" ++ line ++ "$")
        |> String.replace "." "$.^"
        |> String.replace "^$" ""
        |> String.split "^"
        |> List.concatMap (String.split "$" >> List.filter (String.isEmpty >> not))
        |> List.foldl
            (\str ->
                \( lengthSum, list ) ->
                    ( lengthSum + String.length str
                    , ( str
                      , ( lengthSum, lengthSum + String.length str )
                      )
                        :: list
                    )
            )
            ( 0, [] )
        |> Tuple.second
        |> List.filter (Tuple.first >> String.all Char.isDigit)
        |> List.map Tuple.second
        |> Debug.log "indexed"


solve5 : String -> Int
solve5 multiline =
    let
        preProcessed : String
        preProcessed =
            multiline |> identity

        validIndicies : List ( Int, Int )
        validIndicies =
            preProcessed
                |> String.lines
                |> List.indexedMap Tuple.pair
                |> List.concatMap (\( row, line ) -> getValidIndices symbols row line)
                |> List.sortBy Tuple.first
                |> Debug.log "Valid indices: "

        toSymbols : String -> Set String
        toSymbols line =
            line
                |> String.filter (Char.isDigit >> not)
                |> String.replace "." ""
                |> String.split ""
                |> Set.fromList

        symbols : Set String
        symbols =
            preProcessed
                |> String.lines
                |> List.foldl (toSymbols >> Set.union) Set.empty
                |> Debug.log "Symbols: "
    in
    preProcessed
        |> toDottedLines symbols
        |> List.concatMap
            (\( idxR, row ) ->
                row
                    |> getDigitIndices
                    |> List.filter
                        (\( idxFirst, idxLast ) ->
                            validIndicies
                                |> List.filter
                                    (\( idxVrow, _ ) ->
                                        idxVrow == idxR
                                    )
                                |> List.map Tuple.second
                                |> List.any (\i -> idxFirst <= i && i < idxLast)
                        )
                    |> List.filterMap
                        (\( from, to ) ->
                            row
                                |> String.slice from to
                                |> String.toInt
                        )
            )
        |> List.sum


getValidIndices : Set String -> Int -> String -> List ( Int, Int )
getValidIndices symbols row line =
    line
        |> transformSymbols symbols
        |> String.indexes "*"
        |> List.concatMap
            (\i ->
                [ ( row - 1, i - 1 )
                , ( row - 1, i )
                , ( row - 1, i + 1 )
                , ( row, i - 1 )
                , ( row, i + 1 )
                , ( row + 1, i - 1 )
                , ( row + 1, i )
                , ( row + 1, i + 1 )
                ]
            )


transformSymbols : Set String -> String -> String
transformSymbols symbols text =
    symbols
        |> Set.foldl (\s -> String.replace s "*") text


solve6 : String -> Int
solve6 multiline =
    let
        preProcessed : String
        preProcessed =
            multiline |> identity

        getValidIndices2 : Set String -> Int -> String -> List ( Int, ( Int, Int ) )
        getValidIndices2 symbolsSet row line =
            line
                |> transformSymbols symbolsSet
                |> String.indexes "*"
                |> List.concatMap
                    (\i ->
                        [ ( row * 12345 + i, ( row - 1, i - 1 ) )
                        , ( row * 12345 + i, ( row - 1, i ) )
                        , ( row * 12345 + i, ( row - 1, i + 1 ) )
                        , ( row * 12345 + i, ( row, i - 1 ) )
                        , ( row * 12345 + i, ( row, i + 1 ) )
                        , ( row * 12345 + i, ( row + 1, i - 1 ) )
                        , ( row * 12345 + i, ( row + 1, i ) )
                        , ( row * 12345 + i, ( row + 1, i + 1 ) )
                        ]
                    )

        validIndicies : List ( Int, ( Int, Int ) )
        validIndicies =
            preProcessed
                |> String.lines
                |> List.indexedMap Tuple.pair
                |> List.concatMap (\( row, line ) -> getValidIndices2 symbols row line)
                |> List.sortBy (Tuple.second >> Tuple.first)
                |> Debug.log "Valid indices: "

        toSymbols : String -> Set String
        toSymbols line =
            line
                |> String.filter (Char.isDigit >> not)
                |> String.replace "." ""
                |> String.split ""
                |> Set.fromList

        symbols : Set String
        symbols =
            preProcessed
                |> String.lines
                |> List.foldl (toSymbols >> Set.union) Set.empty
                |> Debug.log "Symbols: "
    in
    preProcessed
        |> toDottedLines symbols
        |> List.concatMap
            (\( idxR, row ) ->
                row
                    |> getDigitIndices
                    |> List.concatMap
                        (\( idxFirst, idxLast ) ->
                            let
                                number : Int
                                number =
                                    row
                                        |> String.slice idxFirst idxLast
                                        |> String.toInt
                                        |> Maybe.withDefault 1
                            in
                            validIndicies
                                |> List.filter
                                    (\( _, ( idxVrow, _ ) ) ->
                                        idxVrow == idxR
                                    )
                                |> List.filter (\( _, ( _, i ) ) -> idxFirst <= i && i < idxLast)
                                |> List.map
                                    (\( id, _ ) -> ( id, number ))
                        )
            )
        |> (\vwi ->
                List.foldl
                    (\( id, number ) ->
                        \state ->
                            let
                                append : Int -> Int -> ( Int, Set Int ) -> ( Int, Set Int )
                                append index value aState =
                                    let
                                        ( stateId, stateList ) =
                                            aState
                                    in
                                    if stateId == index then
                                        ( stateId, stateList |> Set.insert value )

                                    else
                                        ( stateId, stateList )
                            in
                            state
                                |> List.map (append id number)
                    )
                    (vwi
                        |> List.map (\( i, _ ) -> ( i, Set.empty ))
                    )
                    vwi
           )
        |> List.map (\( _, l ) -> l |> Set.toList)
        |> Set.fromList
        >> Set.toList
        |> List.filter
            (\l -> List.length l > 1)
        |> List.foldl (List.product >> (+))
            0
        |> Debug.log "Product: "


view : Model -> Html Msg
view model =
    main_ []
        [ section []
            [ h2 [] [ text "Day 1" ]
            , textarea [ onInput <| TextChanged Day1 ] []
            , h3 [] [ text "Part 1: " ]
            , viewSolution Problem1 model.input1
            , h3 [] [ text "Part 2: " ]
            , viewSolution Problem2 model.input1
            ]
        , section []
            [ h2 [] [ text "Day 2" ]
            , textarea [ onInput <| TextChanged Day2 ] []
            , h3 [] [ text "Part 1: " ]
            , viewSolution Problem3 model.input2
            , h3 [] [ text "Part 2: " ]
            , viewSolution Problem4 model.input2
            , pre []
                [ model.input2
                    |> String.replace "red" "🏮"
                    |> String.replace "green" "🐉"
                    |> String.replace "blue" "🥶"
                    |> text
                ]
            , pre []
                [ model.input2
                    |> String.lines
                    |> List.map transform
                    |> String.join "\n"
                    |> String.replace "red" "🏮"
                    |> String.replace "green" "🐉"
                    |> String.replace "blue" "🥶"
                    |> text
                ]
            , pre []
                [ model.input2
                    |> String.lines
                    |> List.map
                        (transform
                            >> String.split ", "
                            >> List.filter (String.contains "red")
                            >> List.map (String.filter Char.isDigit)
                            >> String.join ", "
                        )
                    |> String.join "\n"
                    |> text
                ]
            ]
        , section []
            [ h2 [] [ text "Day 3" ]
            , textarea [ onInput <| TextChanged Day3 ] []

            -- , h3 [] [ text "Part 1: " ]
            -- , viewSolution Problem5 model.input3
            , h3 [] [ text "Part 2: " ]
            , viewSolution Problem6 model.input3
            ]
        ]
