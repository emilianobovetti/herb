module PouchDb.AllDocs exposing (request, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import PouchDb exposing (PouchDb, QueryOptions)
import PortCmd exposing (PortCmd)
import Rocket exposing ((=>))


request : PortCmd.ToMessage msg -> PouchDb -> QueryOptions -> PortCmd msg
request toMessage pouchDb options =
    PortCmd.Disposable toMessage <|
        Encode.object
            [ "type" => Encode.string "pouchDbAllDocs"
            , "pouchDb" => pouchDb
            , "options" => PouchDb.encodeQueryOptions options
            ]


decoder : Decoder a -> Decoder (List a)
decoder fieldDecoder =
    Decode.field "doc" fieldDecoder
        |> Decode.list
        |> Decode.field "rows"
