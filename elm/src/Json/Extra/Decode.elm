module Json.Extra.Decode
    exposing
        ( Value
        , Decoder
        , string
        , float
        , date
        , bool
        , int
        , nullable
        , list
        , dict
        , keyValuePairs
        , field
        , at
        , index
        , maybe
        , oneOf
        , decodeString
        , decodeValue
        , map
        , map2
        , map3
        , map4
        , map5
        , map6
        , map7
        , map8
        , lazy
        , value
        , null
        , succeed
        , fail
        , andThen
        )

import Dict exposing (Dict)
import Date exposing (Date)
import Json.Decode


type alias Value =
    Json.Decode.Value


type alias Decoder a =
    Json.Decode.Decoder a



{- Primitives -}


string : Decoder String
string =
    Json.Decode.string


float : Decoder Float
float =
    Json.Decode.float


date : Decoder Date
date =
    andThen (Date.fromTime >> succeed) float


bool : Decoder Bool
bool =
    Json.Decode.bool


int : Decoder Int
int =
    Json.Decode.int



{- Data Structures -}


nullable : Decoder a -> Decoder (Maybe a)
nullable =
    Json.Decode.nullable


list : Decoder a -> Decoder (List a)
list =
    Json.Decode.list


dict : Decoder a -> Decoder (Dict String a)
dict =
    Json.Decode.dict


keyValuePairs : Decoder a -> Decoder (List ( String, a ))
keyValuePairs =
    Json.Decode.keyValuePairs



{- Object Primitives -}


field : String -> Decoder a -> Decoder a
field =
    Json.Decode.field


at : List String -> Decoder a -> Decoder a
at =
    Json.Decode.at


index : Int -> Decoder a -> Decoder a
index =
    Json.Decode.index



{- Inconsistent Structure -}


maybe : Decoder a -> Decoder (Maybe a)
maybe =
    Json.Decode.maybe


oneOf : List (Decoder a) -> Decoder a
oneOf =
    Json.Decode.oneOf



{- Run Decoders -}


decodeString : Decoder a -> String -> Result String a
decodeString =
    Json.Decode.decodeString


decodeValue : Decoder a -> Value -> Result String a
decodeValue =
    Json.Decode.decodeValue



{- Mapping -}


map : (a -> value) -> Decoder a -> Decoder value
map =
    Json.Decode.map


map2 :
    (a -> b -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder value
map2 =
    Json.Decode.map2


map3 :
    (a -> b -> c -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder value
map3 =
    Json.Decode.map3


map4 :
    (a -> b -> c -> d -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder value
map4 =
    Json.Decode.map4


map5 :
    (a -> b -> c -> d -> e -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder value
map5 =
    Json.Decode.map5


map6 :
    (a -> b -> c -> d -> e -> f -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder f
    -> Decoder value
map6 =
    Json.Decode.map6


map7 :
    (a -> b -> c -> d -> e -> f -> g -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder f
    -> Decoder g
    -> Decoder value
map7 =
    Json.Decode.map7


map8 :
    (a -> b -> c -> d -> e -> f -> g -> h -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder f
    -> Decoder g
    -> Decoder h
    -> Decoder value
map8 =
    Json.Decode.map8



{- Fancy Decoding -}


lazy : (() -> Decoder a) -> Decoder a
lazy =
    Json.Decode.lazy


value : Decoder Value
value =
    Json.Decode.value


null : a -> Decoder a
null =
    Json.Decode.null


succeed : a -> Decoder a
succeed =
    Json.Decode.succeed


fail : String -> Decoder a
fail =
    Json.Decode.fail


andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen =
    Json.Decode.andThen
