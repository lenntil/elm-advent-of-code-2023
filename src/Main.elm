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



-- update


update : Msg -> Model -> Model
update msg model =
    case msg of
        TextChanged input ->
            { model | input = input }


parse : String -> Maybe String
parse input =
    input
        |> String.lines
        |> List.filter (String.isEmpty >> not)
        |> List.map processLine
        |> List.foldl (Maybe.map2 (+)) (Just 0)
        |> Maybe.map String.fromInt


processLine : String -> Maybe Int
processLine line =
    line
        |> String.filter Char.isDigit
        |> extractFirstLastDigits
        |> String.toInt


extractFirstLastDigits : String -> String
extractFirstLastDigits text =
    String.left 1 text ++ String.right 1 text


alphabetsToNumber : List ( String, String )
alphabetsToNumber =
    [ ( "one", "1" ), ( "two", "2" ), ( "three", "3" ), ( "four", "4" ), ( "five", "5" ), ( "six", "6" ), ( "seven", "7" ), ( "eight", "8" ), ( "nine", "9" ) ]


substitute : List ( String, String ) -> String -> String
substitute substitutionTable text =
    substitutionTable
        |> List.map
            (\( first, second ) ->
                String.replace first
                    (String.left 1 first ++ second ++ String.right 1 first)
            )
        |> List.foldl (<|) text


preParse : List ( String, String ) -> String -> String
preParse substitutionTable text =
    String.trim text
        |> substitute substitutionTable



-- view


toStatusMessage : Maybe String -> String
toStatusMessage solution =
    case solution of
        Just a ->
            "Result: " ++ a

        Nothing ->
            "Error: Invalid input!"


viewSolution : Int -> String -> Html Msg
viewSolution problem input =
    text <|
        case input of
            "" ->
                ""

            a ->
                toStatusMessage <|
                    case problem of
                        1 ->
                            a
                                |> parse

                        2 ->
                            a
                                |> preParse alphabetsToNumber
                                |> parse

                        _ ->
                            Just "Error: No problem exist."


view : Model -> Html Msg
view model =
    div []
        [ textarea [ onInput TextChanged ] []
        , div []
            [ div [] [ h3 [] [ text "Part 1: " ] ]
            , viewSolution 1 model.input
            ]
        , div []
            [ div [] [ h3 [] [ text "Part 2: " ] ]
            , viewSolution 2 model.input
            ]
        ]
