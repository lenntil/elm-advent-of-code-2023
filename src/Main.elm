module Main exposing (Part, main)

import Browser
import Day5
import Html exposing (..)
import Html.Attributes exposing (required, selected, style, value)
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
    , day : Day
    , part : Part
    }


init : Model
init =
    { input = ""
    , day = Day5
    , part = Part2
    }


type Msg
    = TextChanged String
    | DaySelected String
    | PartSelected String


type Day
    = Day1
    | Day2
    | Day3
    | Day4
    | Day5
    | Day6
    | Day7
    | Day8
    | Day9
    | Day10
    | Day11
    | Day12
    | Day13
    | Day14
    | Day15
    | Day16
    | Day17
    | Day18
    | Day19
    | Day20
    | Day21
    | Day22
    | Day23
    | Day24
    | Day25


type Part
    = Part1
    | Part2


toDay : Int -> Day
toDay x =
    case x of
        1 ->
            Day1

        2 ->
            Day2

        3 ->
            Day3

        4 ->
            Day4

        5 ->
            Day5

        6 ->
            Day6

        7 ->
            Day7

        8 ->
            Day8

        9 ->
            Day9

        10 ->
            Day10

        11 ->
            Day11

        12 ->
            Day12

        13 ->
            Day13

        14 ->
            Day14

        15 ->
            Day15

        16 ->
            Day16

        17 ->
            Day17

        18 ->
            Day18

        19 ->
            Day19

        20 ->
            Day20

        21 ->
            Day21

        22 ->
            Day22

        23 ->
            Day23

        24 ->
            Day24

        25 ->
            Day25

        _ ->
            Day1


toPart : Int -> Part
toPart x =
    case x of
        1 ->
            Part1

        2 ->
            Part2

        _ ->
            Part1



-- update


update : Msg -> Model -> Model
update msg model =
    case msg of
        TextChanged input ->
            { model | input = input }

        DaySelected day ->
            case String.toInt day of
                Nothing ->
                    model

                Just x ->
                    { model | day = toDay x }

        PartSelected part ->
            case String.toInt part of
                Nothing ->
                    model

                Just x ->
                    { model | part = toPart x }



-- view


toStatusMessage : String -> String
toStatusMessage solution =
    case solution of
        "" ->
            "Error: Invalid input!"

        a ->
            "Result: " ++ a


viewSolution : Day -> Part -> String -> Html Msg
viewSolution day part input =
    p [ style "white-space" "pre-wrap" ]
        [ text <|
            case input of
                "" ->
                    "Waiting for input..."

                a ->
                    a
                        |> (case day of
                                Day5 ->
                                    case part of
                                        Part1 ->
                                            Day5.solvePart1

                                        Part2 ->
                                            Day5.solvePart2

                                _ ->
                                    identity
                           )
                        |> toStatusMessage
        ]


view : Model -> Html Msg
view model =
    let
        initial_selection =
            { day = 5
            , part = 1
            }
    in
    main_ []
        [ select [ required True, onInput DaySelected ]
            (List.map
                (\x ->
                    option
                        [ value <| String.fromInt x
                        , if x == initial_selection.day then
                            selected True

                          else
                            selected False
                        ]
                        [ text <| "Day " ++ String.fromInt x ]
                )
                (List.range 1 25)
            )
        , select [ required True, onInput PartSelected ]
            [ option
                [ value <| "1"
                , if initial_selection.part == 1 then
                    selected True

                  else
                    selected False
                ]
                [ text <| "Part 1" ]
            , option
                [ value <| "2"
                , if initial_selection.part == 2 then
                    selected True

                  else
                    selected False
                ]
                [ text <| "Part 2" ]
            ]
        , section []
            [ h2 []
                [ text <|
                    Debug.toString model.day
                        ++ " "
                        ++ Debug.toString model.part
                ]
            , textarea [ onInput <| TextChanged ] []
            , viewSolution model.day model.part model.input
            , h3 []
                [ text "Textbox with no functionality"
                ]
            , textarea [] []
            ]
        ]
