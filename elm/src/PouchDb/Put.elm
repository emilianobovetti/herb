module PouchDb.Put exposing (request)

import Json.Encode as Encode exposing (Value)
import PortCmd exposing (PortCmd)
import PouchDb exposing (PouchDb)
import Rocket exposing ((=>))


request : PortCmd.ToMessage msg -> PouchDb -> Value -> PortCmd msg
request toMessage pouchDb document =
    PortCmd.Disposable toMessage <|
        Encode.object
            [ "type" => Encode.string "pouchDbPut"
            , "pouchDb" => pouchDb
            , "document" => document
            ]
