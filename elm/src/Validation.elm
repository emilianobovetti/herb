module Validation
    exposing
        ( TypeError
            ( Missing
            , InvalidString
            , InvalidFloat
            , InvalidBool
            , InvalidDate
            , InvalidInt
            , NonPositive
            , Negative
            )
        , Container
            ( String
            , Float
            , Date
            , Bool
            , Int
            , Empty
            )
        , Errors
        , Data
        , ValidationResult
        , Validation
        , Validator
        , validate
        , with
        , end
        , foldResults
        , string
        , float
        , bool
        , int
        , maybe
        , nonNegativeFloat
        , nonNegativeInt
        , positiveFloat
        , positiveInt
        , filterString
        , getMaybeString
        , getMaybeFloat
        , getMaybeDate
        , getMaybeBool
        , getMaybeInt
        , getString
        , getFloat
        , getDate
        , getBool
        , getInt
        )

import Json.Decode as Decode
import Dict exposing (Dict)
import Date exposing (Date)


type TypeError
    = Missing
    | InvalidString
    | InvalidFloat
    | InvalidBool
    | InvalidDate
    | InvalidInt
    | NonPositive
    | Negative


type Container
    = String String
    | Float Float
    | Date Date
    | Bool Bool
    | Int Int
    | Empty


type alias Errors =
    Dict String TypeError


type alias Data =
    Dict String Container


type alias ValidationResult =
    Result Errors Data


type alias Validation input =
    ( input, ValidationResult )


type alias Key =
    String


type alias FieldGetter input =
    String -> input -> Result TypeError String


type alias Validator =
    Result TypeError String -> Result TypeError Container


validate : input -> Validation input
validate input =
    ( input, Ok Dict.empty )


with : FieldGetter input -> Key -> Validator -> Validation input -> Validation input
with fieldGetter key validator ( input, validation ) =
    case ( validation, validator (fieldGetter key input) ) of
        ( Ok data, Ok container ) ->
            ( input, Ok (Dict.insert key container data) )

        ( Ok _, Err error ) ->
            ( input, Err (Dict.singleton key error) )

        ( Err errors, Ok _ ) ->
            ( input, Err errors )

        ( Err errors, Err error ) ->
            ( input, Err (Dict.insert key error errors) )


end : Validation input -> ValidationResult
end validation =
    Tuple.second validation


foldResults : List (Result Errors value) -> Result Errors (List value)
foldResults results =
    let
        loop : List (Result Errors value) -> Errors -> List value -> Result Errors (List value)
        loop list errors values =
            case list of
                (Err err) :: tl ->
                    loop tl (Dict.union errors err) values

                (Ok val) :: tl ->
                    loop tl errors (val :: values)

                [] ->
                    if Dict.isEmpty errors then
                        Ok values
                    else
                        Err errors
    in
        loop results Dict.empty []



{- Validators -}


string : Validator
string =
    String
        >> Ok
        |> Result.andThen


float : Validator
float =
    Decode.decodeString Decode.float
        >> Result.map Float
        >> Result.mapError (always InvalidFloat)
        |> Result.andThen


bool : Validator
bool =
    Decode.decodeString Decode.bool
        >> Result.map Bool
        >> Result.mapError (always InvalidBool)
        |> Result.andThen


int : Validator
int =
    Decode.decodeString Decode.int
        >> Result.map Int
        >> Result.mapError (always InvalidInt)
        |> Result.andThen


maybe : Validator -> Validator
maybe validator result =
    case result of
        Err Missing ->
            Ok Empty

        Err error ->
            Err error

        Ok value ->
            validator (Ok value)


nonNegativeFloat : Validator
nonNegativeFloat result =
    case float result of
        Ok (Float float) ->
            if float < 0 then
                Err Negative
            else
                Ok (Float float)

        Ok _ ->
            Err InvalidFloat

        Err error ->
            Err error


nonNegativeInt : Validator
nonNegativeInt result =
    case int result of
        Ok (Int int) ->
            if int < 0 then
                Err Negative
            else
                Ok (Int int)

        Ok _ ->
            Err InvalidInt

        Err error ->
            Err error


positiveFloat : Validator
positiveFloat result =
    case float result of
        Ok (Float float) ->
            if float > 0 then
                Ok (Float float)
            else
                Err NonPositive

        Ok _ ->
            Err InvalidFloat

        Err error ->
            Err error


positiveInt : Validator
positiveInt result =
    case int result of
        Ok (Int int) ->
            if int > 0 then
                Ok (Int int)
            else
                Err NonPositive

        Ok _ ->
            Err InvalidInt

        Err error ->
            Err error


filterString : (String -> Bool) -> Result TypeError Container -> Result TypeError Container
filterString filterFn result =
    case result of
        Ok (String val) ->
            if filterFn val then
                Ok (String val)
            else
                Err InvalidString

        _ ->
            result



{- Maybe Getters -}


type alias MaybeGetter value =
    Key -> Data -> Maybe value


getMaybe : MaybeGetter Container
getMaybe key data =
    Dict.get key data


getMaybeString : MaybeGetter String
getMaybeString key result =
    case getMaybe key result of
        Just (String string) ->
            Just (String.trim string)

        _ ->
            Nothing


getMaybeFloat : MaybeGetter Float
getMaybeFloat key result =
    case getMaybe key result of
        Just (Float float) ->
            Just float

        _ ->
            Nothing


getMaybeDate : MaybeGetter Date
getMaybeDate key result =
    case getMaybe key result of
        Just (Date date) ->
            Just date

        _ ->
            Nothing


getMaybeBool : MaybeGetter Bool
getMaybeBool key result =
    case getMaybe key result of
        Just (Bool bool) ->
            Just bool

        _ ->
            Nothing


getMaybeInt : MaybeGetter Int
getMaybeInt key result =
    case getMaybe key result of
        Just (Int int) ->
            Just int

        _ ->
            Nothing



{- Getters -}


type alias Getter value =
    String -> Data -> value


getString : Getter String
getString key result =
    Maybe.withDefault "" (getMaybeString key result)


getFloat : Getter Float
getFloat key result =
    Maybe.withDefault 0 (getMaybeFloat key result)


getDate : Getter Date
getDate key result =
    Maybe.withDefault (Date.fromTime 0) (getMaybeDate key result)


getBool : Getter Bool
getBool key result =
    Maybe.withDefault False (getMaybeBool key result)


getInt : Getter Int
getInt key result =
    Maybe.withDefault 0 (getMaybeInt key result)
