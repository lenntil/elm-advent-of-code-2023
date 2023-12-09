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


type alias Model =
    { input : String
    }


init : Model
init =
    { input = ""
    }


type Msg
    = TextChanged String


update : Msg -> Model -> Model
update msg model =
    case msg of
        TextChanged input ->
            { model | input = input }


solve : String -> Maybe Int
solve input =
    input
        |> String.lines
        |> List.filter (String.isEmpty >> not)
        |> List.map processLine
        |> List.foldl (Maybe.map2 (+)) (Just 0)


processLine : String -> Maybe Int
processLine line =
    line
        |> String.filter Char.isDigit
        |> (\s -> String.left 1 s ++ String.right 1 s)
        |> String.toInt


view : Model -> Html Msg
view model =
    div []
        [ textarea [ onInput TextChanged ] [ text model.input ]
        , if String.isEmpty model.input then
            text ""

          else
            div []
                [ case solve model.input of
                    Just a ->
                        text <| "Result: " ++ String.fromInt a

                    Nothing ->
                        text "Error: Invalid input!"
                ]
        ]
