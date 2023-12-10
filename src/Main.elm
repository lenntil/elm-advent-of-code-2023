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
    { input : String
    }


init : Model
init =
    { input = ""
    }


type Msg
    = TextChanged String


type Problem
    = Problem1
    | Problem2



-- update


update : Msg -> Model -> Model
update msg model =
    case msg of
        TextChanged input ->
            { model | input = input }


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



-- view


toStatusMessage : Int -> String
toStatusMessage solution =
    case solution of
        0 ->
            "Error: Invalid input!"

        a ->
            "Result: " ++ String.fromInt a


viewSolution : Problem -> String -> Html Msg
viewSolution problem input =
    text <|
        case input of
            "" ->
                "Waiting for input..."

            a ->
                case problem of
                    Problem1 ->
                        a
                            |> parseToInt
                            |> toStatusMessage

                    Problem2 ->
                        a
                            |> preParse alphabetsToNumber
                            |> parseToInt
                            |> toStatusMessage


view : Model -> Html Msg
view model =
    div []
        [ textarea [ onInput TextChanged ] []
        , div []
            [ div [] [ h3 [] [ text "Part 1: " ] ]
            , viewSolution Problem1 model.input
            ]
        , div []
            [ div [] [ h3 [] [ text "Part 2: " ] ]
            , viewSolution Problem2 model.input
            ]
        ]
