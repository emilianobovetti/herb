module Model.Sale exposing (Sale, idPrefix, create, fromString, toJson, decoder)

import Json.Decode.Pipeline exposing (decode, required)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Time exposing (Time)
import Dict exposing (Dict)
import Rocket exposing ((=>))
import PouchDb


type alias Sale =
    { id : PouchDb.Id
    , productCode : String
    , timestamp : Time
    , quantity : Int
    }


idPrefix : String
idPrefix =
    "sale_"


create : String -> Time -> Int -> Sale
create code timestamp quantity =
    { id = idPrefix ++ code ++ "_" ++ toString timestamp
    , productCode = code
    , timestamp = timestamp
    , quantity = quantity
    }


fromString : Time -> Maybe String -> List Sale
fromString timestamp input =
    let
        strToCode : String -> Maybe String
        strToCode string =
            case String.trim string of
                "" ->
                    Nothing

                code ->
                    Just code

        updateDict : String -> Dict String Int -> Dict String Int
        updateDict code dict =
            Dict.update code (Maybe.withDefault 0 >> (+) 1 >> Just) dict
    in
        Maybe.withDefault "" input
            |> String.split "\n"
            |> List.filterMap strToCode
            |> List.foldl updateDict Dict.empty
            |> Dict.toList
            |> List.map (\( code, quantity ) -> create code timestamp quantity)


toJson : Sale -> Encode.Value
toJson sale =
    Encode.object
        [ "_id" => Encode.string sale.id
        , "quantity" => Encode.int sale.quantity
        ]


fromData : String -> Int -> Sale
fromData id quantity =
    let
        ( code, timestamp ) =
            case String.split "_" id of
                _ :: code_ :: ts :: _ ->
                    ( code_, String.toFloat ts |> Result.withDefault 0 )

                _ ->
                    ( id, 0 )
    in
        Sale id code timestamp quantity


decoder : Decoder Sale
decoder =
    decode fromData
        |> required "_id" Decode.string
        |> required "quantity" Decode.int
