module PouchDb.BulkDocs exposing (request)

import Json.Encode as Encode exposing (Value)
import PouchDb exposing (PouchDb)
import PortCmd exposing (PortCmd)
import Rocket exposing ((=>))


request : PortCmd.ToMessage msg -> PouchDb -> List Value -> PortCmd msg
request toMessage pouchDb documents =
    PortCmd.Disposable toMessage <|
        Encode.object
            [ "type" => Encode.string "pouchDbBulkDocs"
            , "pouchDb" => pouchDb
            , "documents" => Encode.list documents
            ]
