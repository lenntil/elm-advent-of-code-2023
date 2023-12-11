module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (onInput)


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



-- | Problem5
-- | Problem6
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
                                Debug.todo

                            Problem6 ->
                                Debug.todo

                            Problem7 ->
                                Debug.todo

                            Problem8 ->
                                Debug.todo
                       )
                    |> Debug.toString
                    |> toStatusMessage


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
            , h3 [] [ text "part 1: " ]
            , viewSolution Problem3 model.input2
            , h3 [] [ text "part 2: " ]
            , viewSolution Problem4 model.input2
            , pre [] [ text model.input2 ]
            , pre []
                [ model.input2 |> String.lines |> List.map transform |> String.join "\n" |> text ]
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
            , h3 [] [ text "Part 1: " ]
            , viewSolution Problem5 model.input1
            , h3 [] [ text "Part 2: " ]
            , viewSolution Problem6 model.input1
            ]
        , section []
            [ h2 [] [ text "Day 4" ]
            , textarea [ onInput <| TextChanged Day4 ] []
            , h3 [] [ text "Part 1: " ]
            , viewSolution Problem7 model.input1
            , h3 [] [ text "Part 2: " ]
            , viewSolution Problem8 model.input1
            ]
        ]
