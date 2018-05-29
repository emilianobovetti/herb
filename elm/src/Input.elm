module Input
    exposing
        ( Input
        , onSubmit
        , getInput
        , jsonToInput
        , jsonResultToInput
        , empty
        , get
        , remove
        , getValue
        , setValue
        , getWithDefault
        , getError
        , setError
        , setValidationErrors
        , mapValue
        , keys
        , indexes
        , validate
        , with
        , end
        )

import Validation exposing (Validation, TypeError(Missing))
import Json.Decode as Decode exposing (decodeValue)
import Json.Encode as Encode exposing (Value, null)
import PortCmd exposing (PortCmd)
import Regex exposing (Regex)
import Rocket exposing ((=>))
import Dict exposing (Dict)
import List.Extra


onSubmit : (Input -> msg) -> String -> PortCmd msg
onSubmit toMessage id =
    PortCmd.Persistent (jsonResultToInput >> toMessage) <|
        Encode.object
            [ "type" => Encode.string "onSubmit"
            , "id" => Encode.string id
            ]


getInput : (Input -> msg) -> String -> PortCmd msg
getInput toMessage id =
    PortCmd.Disposable (jsonResultToInput >> toMessage) <|
        Encode.object
            [ "type" => Encode.string "getInput"
            , "id" => Encode.string id
            ]


jsonToInput : Value -> Input
jsonToInput value =
    case decodeValue (Decode.dict Decode.string) value of
        Err _ ->
            empty

        Ok data ->
            Input ( data, Dict.empty )


jsonResultToInput : Result Value Value -> Input
jsonResultToInput result =
    result
        |> Result.withDefault null
        |> jsonToInput


type Input
    = Input ( Dict String String, Validation.Errors )


empty : Input
empty =
    Input ( Dict.empty, Dict.empty )


get : String -> Input -> Result TypeError String
get key ((Input ( _, errors )) as input) =
    case ( Dict.get key errors, getValue key input ) of
        ( Just error, _ ) ->
            Err error

        ( Nothing, Nothing ) ->
            Err Missing

        ( Nothing, Just str ) ->
            Ok str


remove : String -> Input -> Input
remove key (Input ( data, errors )) =
    Input ( Dict.remove key data, Dict.remove key errors )


getValue : String -> Input -> Maybe String
getValue key (Input ( data, _ )) =
    case Dict.get key data of
        Nothing ->
            Nothing

        Just str ->
            if String.trim str == "" then
                Nothing
            else
                Just str


getWithDefault : String -> String -> Input -> String
getWithDefault default key input =
    getValue key input
        |> Maybe.withDefault default


setValue : String -> String -> Input -> Input
setValue key value (Input ( data, errors )) =
    Input ( Dict.insert key value data, Dict.remove key errors )


getError : String -> Input -> Maybe TypeError
getError key (Input ( _, errors )) =
    Dict.get key errors


setError : String -> TypeError -> Input -> Input
setError key error (Input ( data, errors )) =
    Input ( data, Dict.insert key error errors )


setValidationErrors : Validation.Errors -> Input -> Input
setValidationErrors errors (Input ( data, _ )) =
    Input ( data, errors )


mapValue : String -> (String -> String) -> Input -> Input
mapValue key mapFn (Input ( data, errors )) =
    Input ( Dict.update key (Maybe.map mapFn) data, errors )


keys : Input -> List String
keys (Input ( data, _ )) =
    Dict.keys data


startsWithNum : Regex
startsWithNum =
    Regex.regex "^(\\d+)"


indexes : Input -> List Int
indexes input =
    let
        keysToIndexes : String -> Result String Int
        keysToIndexes line =
            line
                |> Regex.find (Regex.AtMost 1) startsWithNum
                |> List.head
                |> Maybe.map .match
                |> Result.fromMaybe "Missing"
                |> Result.andThen String.toInt
    in
        keys input
            |> List.map keysToIndexes
            |> List.filterMap Result.toMaybe
            |> List.Extra.unique



{- Validation shortcut -}


validate : Input -> Validation Input
validate =
    Validation.validate


with : String -> Validation.Validator -> Validation Input -> Validation Input
with =
    Validation.with get


end : Validation Input -> Validation.ValidationResult
end =
    Validation.end
