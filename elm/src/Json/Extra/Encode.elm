module Json.Extra.Encode
    exposing
        ( Value
        , encode
        , string
        , float
        , date
        , bool
        , int
        , null
        , maybeString
        , maybeInt
        , maybeFloat
        , maybeBool
        , list
        , object
        )

import Date exposing (Date)
import Json.Encode


{- Encoding -}


type alias Value =
    Json.Encode.Value


encode : Int -> Value -> String
encode =
    Json.Encode.encode



{- Primitives -}


string : String -> Value
string =
    Json.Encode.string


float : Float -> Value
float =
    Json.Encode.float


date : Date -> Value
date date =
    float (Date.toTime date)


bool : Bool -> Value
bool =
    Json.Encode.bool


int : Int -> Value
int =
    Json.Encode.int


null : Value
null =
    Json.Encode.null



{- Nullable -}


maybeString : Maybe String -> Value
maybeString value =
    Maybe.map string value
        |> Maybe.withDefault null


maybeInt : Maybe Int -> Value
maybeInt value =
    Maybe.map int value
        |> Maybe.withDefault null


maybeFloat : Maybe Float -> Value
maybeFloat value =
    Maybe.map float value
        |> Maybe.withDefault null


maybeBool : Maybe Bool -> Value
maybeBool value =
    Maybe.map bool value
        |> Maybe.withDefault null



{- List -}


list : List Value -> Value
list =
    Json.Encode.list



{- Objects -}


object : List ( String, Value ) -> Value
object =
    Json.Encode.object
