module Model.Stock exposing (Stock, idPrefix, create, validate, toJson, decoder)

import Validation exposing (getString, getInt)
import Input exposing (Input, with)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Time exposing (Time)
import Rocket exposing ((=>))
import PouchDb


type alias Stock =
    { id : PouchDb.Id
    , productCode : String
    , timestamp : Time
    , quantity : Int
    }


idPrefix : String
idPrefix =
    "stock_"


create : String -> Time -> Int -> Stock
create code timestamp quantity =
    { id = idPrefix ++ code ++ "_" ++ toString timestamp
    , productCode = code
    , timestamp = timestamp
    , quantity = quantity
    }


validate : Time -> Input -> Result Validation.Errors Stock
validate timestamp input =
    let
        result : Validation.ValidationResult
        result =
            Input.validate input
                |> with "productCode" Validation.string
                |> with "quantity" Validation.positiveInt
                |> Input.end

        dataToStock : Validation.Data -> Stock
        dataToStock data =
            let
                code : String
                code =
                    getString "productCode" data

                quantity : Int
                quantity =
                    getInt "quantity" data
            in
                create code timestamp quantity
    in
        Result.map dataToStock result


toJson : Stock -> Encode.Value
toJson stock =
    Encode.object
        [ "_id" => Encode.string stock.id
        , "quantity" => Encode.int stock.quantity
        ]


createFromJson : String -> Int -> Stock
createFromJson id quantity =
    let
        ( code, timestamp ) =
            case String.split "_" id of
                _ :: code_ :: ts :: _ ->
                    ( code_, Result.withDefault 0 (String.toFloat ts) )

                _ ->
                    ( id, 0 )
    in
        Stock id code timestamp quantity


decoder : Decoder Stock
decoder =
    decode createFromJson
        |> required "_id" Decode.string
        |> required "quantity" Decode.int
