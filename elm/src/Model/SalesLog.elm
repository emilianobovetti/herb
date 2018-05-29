module Model.SalesLog exposing (SalesLog, idPrefix, toJson, decoder)

import Json.Decode.Pipeline exposing (decode, required)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Rocket exposing ((=>))
import Sale exposing (Sale)
import PouchDb


type alias SalesLog =
    { id : PouchDb.Id
    , sales : List Sale
    }


idPrefix : String
idPrefix =
    "salesLog_"


saleEncoder : Sale -> Encode.Value
saleEncoder sale =
    Encode.object
        [ "timestamp" => Encode.float sale.timestamp
        , "quantity" => Encode.int sale.quantity
        ]


toJson : SalesLog -> Encode.Value
toJson salesLog =
    Encode.object
        [ "_id" => Encode.string salesLog.id
        , "sales" => Encode.list (List.map saleEncoder salesLog.sales)
        ]


saleDecoder : Decoder Sale
saleDecoder =
    decode Sale
        |> required "timestamp" Decode.float
        |> required "quantity" Decode.int


decoder : Decoder SalesLog
decoder =
    decode SalesLog
        |> required "_id" Decode.string
        |> required "sales" (Decode.list saleDecoder)
