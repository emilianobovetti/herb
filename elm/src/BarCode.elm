module BarCode exposing (normalize)

import Rocket exposing ((=>))
import Dict exposing (Dict)


sixCharCodes : Dict Char Int
sixCharCodes =
    [ '0' => 0
    , '1' => 1
    , '2' => 2
    , '3' => 3
    , '4' => 4
    , '5' => 5
    , '6' => 6
    , '7' => 7
    , '8' => 8
    , '9' => 9
    , 'B' => 10
    , 'C' => 11
    , 'D' => 12
    , 'F' => 13
    , 'G' => 14
    , 'H' => 15
    , 'J' => 16
    , 'K' => 17
    , 'L' => 18
    , 'M' => 19
    , 'N' => 20
    , 'P' => 21
    , 'Q' => 22
    , 'R' => 23
    , 'S' => 24
    , 'T' => 25
    , 'U' => 26
    , 'V' => 27
    , 'W' => 28
    , 'X' => 29
    , 'Y' => 30
    , 'Z' => 31
    ]
        |> Dict.fromList


parseSixCharCode : String -> Maybe String
parseSixCharCode str =
    let
        stringLength : Int
        stringLength =
            String.length str

        codeList : List Int
        codeList =
            String.toList str
                |> List.reverse
                |> List.filterMap (flip Dict.get sixCharCodes)

        codeListLength : Int
        codeListLength =
            List.length codeList
    in
        if stringLength == 6 && stringLength == codeListLength then
            codeList
                |> List.indexedMap (\index x -> 2 ^ (index * 5) * x)
                |> List.sum
                |> toString
                |> Just
        else
            Nothing


normalize : String -> String
normalize code =
    let
        trimCode : String
        trimCode =
            String.trim code
    in
        trimCode
            |> String.toUpper
            |> parseSixCharCode
            |> Maybe.withDefault trimCode
