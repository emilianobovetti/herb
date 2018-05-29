module PouchDb.Create
    exposing
        ( AjaxOptions
        , AuthOptions
        , Adapter(IndexedDb, LevelDb, WebSql, Http)
        , CreateOptions(Local, Remote)
        , request
        , localDefaults
        , indexedDbDefaults
        , levelDbDefaults
        , webSqlDefaults
        , httpDefaults
        , remoteDefaults
        , revsLimit
        , autoCompaction
        , ajax
        , auth
        )

import PortHandler exposing (PortMsg)
import Rocket exposing ((=>))


type alias AjaxOptions =
    { timeout : Int
    , cache : Bool
    , headers : String
    , withCredentials : Bool
    }


type alias AuthOptions =
    { username : String
    , password : String
    }


type Adapter
    = IndexedDb
    | LevelDb
    | WebSql
    | Http


adapterToString : Adapter -> String
adapterToString adapter =
    case adapter of
        IndexedDb ->
            "idb"

        LevelDb ->
            "leveldb"

        WebSql ->
            "websql"

        Http ->
            "http"


type CreateOptions
    = Local
        { name : String
        , adapter : Adapter
        , revsLimit : Maybe Int
        , autoCompaction : Maybe Bool
        }
    | Remote
        { name : String
        , ajax : Maybe AjaxOptions
        , auth : Maybe AuthOptions
        , skipSetup : Maybe Bool
        }


ajaxOptionsEncoder : Maybe AjaxOptions -> Value
ajaxOptionsEncoder options =
    case options of
        Just opt ->
            Encode.object
                [ "timeout" => Encode.int opt.timeout
                , "cache" => Encode.bool opt.cache
                , "headers" => Encode.string opt.headers
                , "withCredentials" => Encode.bool opt.withCredentials
                ]

        Nothing ->
            Encode.null


authOptionsEncoder : Maybe AuthOptions -> Value
authOptionsEncoder options =
    case options of
        Just opt ->
            Encode.object
                [ "username" => Encode.string opt.username
                , "password" => Encode.string opt.password
                ]

        Nothing ->
            Encode.null


request : CreateOptions -> PortMsg
request options =
    case options of
        Local { name, adapter, revsLimit, autoCompaction } ->
            PortHandler.Disposable <|
                Encode.object
                    [ "type" => Encode.string "pouchDbCreateLocal"
                    , "name" => Encode.string name
                    , "adapter" => adapterToString adapter |> Encode.string
                    , "revs_limit" => Encode.maybeInt revsLimit
                    , "auto_compaction" => Encode.maybeBool autoCompaction
                    ]

        Remote { name, ajax, auth, skipSetup } ->
            PortHandler.Disposable <|
                Encode.object
                    [ "type" => Encode.string "pouchDbCreateRemote"
                    , "name" => Encode.string name
                    , "ajax" => ajaxOptionsEncoder ajax
                    , "auth" => authOptionsEncoder auth
                    , "skip_setup" => Encode.maybeBool skipSetup
                    ]


localDefaults : String -> Adapter -> CreateOptions
localDefaults name adapter =
    Local
        { name = name
        , adapter = adapter
        , revsLimit = Nothing
        , autoCompaction = Nothing
        }


indexedDbDefaults : String -> CreateOptions
indexedDbDefaults name =
    localDefaults name IndexedDb


levelDbDefaults : String -> CreateOptions
levelDbDefaults name =
    localDefaults name LevelDb


webSqlDefaults : String -> CreateOptions
webSqlDefaults name =
    localDefaults name WebSql


httpDefaults : String -> CreateOptions
httpDefaults name =
    localDefaults name Http


remoteDefaults : String -> CreateOptions
remoteDefaults name =
    Remote
        { name = name
        , ajax = Nothing
        , auth = Nothing
        , skipSetup = Nothing
        }


revsLimit : Int -> { options | revsLimit : Maybe Int } -> { options | revsLimit : Maybe Int }
revsLimit limit options =
    { options | revsLimit = Just limit }


autoCompaction : { options | autoCompaction : Maybe Bool } -> { options | autoCompaction : Maybe Bool }
autoCompaction options =
    { options | autoCompaction = Just True }


ajax : Int -> Bool -> String -> Bool -> { options | ajax : Maybe AjaxOptions } -> { options | ajax : Maybe AjaxOptions }
ajax timeout cache headers withCredentials options =
    { options
        | ajax =
            { timeout = timeout
            , cache = cache
            , headers = headers
            , withCredentials = withCredentials
            }
    }


auth : String -> String -> { options | auth : Maybe AuthOptions } -> { options | auth : Maybe AuthOptions }
auth username password =
    { options
        | auth =
            { username = username
            , password = password
            }
    }
