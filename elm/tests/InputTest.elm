module InputTest exposing (..)

import TypeError exposing (TypeError(Missing, InvalidString))
import Input exposing (Input)
import Test exposing (Test, describe, test)
import Expect


keyValue : Input
keyValue =
    Input.setValue "key" "value" Input.empty


keyError : Input
keyError =
    Input.setError "key" InvalidString Input.empty


keyValueError : Input
keyValueError =
    Input.setValue "key" "value" Input.empty
        |> Input.setError "key" InvalidString


keyEmptyString : Input
keyEmptyString =
    Input.setValue "key" "" Input.empty


getTests : Test
getTests =
    describe "Input.get"
        [ test "should return `Err Missing` from an empty input" <|
            \() ->
                Expect.equal (Err Missing) (Input.get "key" Input.empty)
        , test "should return the corresponding value when exists" <|
            \() ->
                Expect.equal (Ok "value") (Input.get "key" keyValue)
        , test "should return the correct error when setted" <|
            \() ->
                Expect.equal (Err InvalidString) (Input.get "key" keyError)
        , test "should return the correct error when both value and error are setted" <|
            \() ->
                Expect.equal (Err InvalidString) (Input.get "key" keyValueError)
        , test "should return `Err Missing` when the key doesn't exist" <|
            \() ->
                Expect.equal (Err Missing) (Input.get "non-existing" keyValue)
        , test "should return `Err Missing` when value is an empty string" <|
            \() ->
                Expect.equal (Err Missing) (Input.get "key" keyEmptyString)
        ]
