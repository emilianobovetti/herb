module PouchDb.Query exposing (request)

import PouchDb exposing (PouchDb, QueryOptions)
import PortCmd exposing (PortCmd)
import Json.Encode as Encode
import Rocket exposing ((=>))


request : PortCmd.ToMessage msg -> PouchDb -> String -> QueryOptions -> PortCmd msg
request toMessage pouchDb viewName options =
    PortCmd.Disposable toMessage <|
        Encode.object
            [ "type" => Encode.string "pouchDbQuery"
            , "pouchDb" => pouchDb
            , "viewName" => Encode.string viewName
            , "options" => PouchDb.encodeQueryOptions options
            ]
