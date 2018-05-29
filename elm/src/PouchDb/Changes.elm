module PouchDb.Changes
    exposing
        ( Since(Now, Since)
        , request
        , decoder
        , defaults
        , live
        , includeDocs
        , conflicts
        , base64
        , binary
        , descending
        , since
        , sinceNow
        , limit
        , timeout
        )

import PouchDb exposing (PouchDb, Attachments(Binary))
import Json.Extra.Decode as Decode exposing (Decoder)
import Json.Extra.Encode as Encode exposing (Value)
import PortCmd exposing (PortCmd)
import Rocket exposing ((=>))


type Since
    = Now
    | Since Int


type alias ChangesOptions =
    { live : Maybe Bool
    , includeDocs : Maybe Bool
    , conflicts : Maybe Bool
    , attachments : Maybe Attachments
    , descending : Maybe Bool
    , since : Maybe Since
    , limit : Maybe Int
    , timeout : Maybe Int
    }


encodeSince : Maybe Since -> Value
encodeSince since =
    case since of
        Just (Since time) ->
            Encode.int time

        Just Now ->
            Encode.string "now"

        Nothing ->
            Encode.null


encodeChangesOptions : ChangesOptions -> Value
encodeChangesOptions options =
    Encode.object
        [ "live" => Encode.maybeBool options.live
        , "include_docs" => Encode.maybeBool options.includeDocs
        , "conflicts" => Encode.maybeBool options.conflicts
        , "attachments" => Encode.bool (options.attachments /= Nothing)
        , "binary" => Encode.bool (options.attachments == Just Binary)
        , "descending" => Encode.maybeBool options.descending
        , "since" => encodeSince options.since
        , "limit" => Encode.maybeInt options.limit
        , "timeout" => Encode.maybeInt options.timeout
        ]


request : PortCmd.ToMessage msg -> PouchDb -> ChangesOptions -> PortCmd msg
request toMessage pouchDb options =
    PortCmd.Persistent toMessage <|
        Encode.object
            [ "type" => Encode.string "pouchDbChanges"
            , "pouchDb" => pouchDb
            , "options" => encodeChangesOptions options
            ]


decoder : Decoder a -> Decoder a
decoder fieldDecoder =
    Decode.field "doc" fieldDecoder


defaults : ChangesOptions
defaults =
    { live = Nothing
    , includeDocs = Nothing
    , conflicts = Nothing
    , attachments = Nothing
    , descending = Nothing
    , since = Nothing
    , limit = Nothing
    , timeout = Nothing
    }


live : { options | live : Maybe Bool } -> { options | live : Maybe Bool }
live options =
    { options | live = Just True }


includeDocs : { options | includeDocs : Maybe Bool } -> { options | includeDocs : Maybe Bool }
includeDocs =
    PouchDb.includeDocs


conflicts : { options | conflicts : Maybe Bool } -> { options | conflicts : Maybe Bool }
conflicts =
    PouchDb.conflicts


base64 : { options | attachments : Maybe Attachments } -> { options | attachments : Maybe Attachments }
base64 =
    PouchDb.base64


binary : { options | attachments : Maybe Attachments } -> { options | attachments : Maybe Attachments }
binary =
    PouchDb.binary


descending : { options | descending : Maybe Bool } -> { options | descending : Maybe Bool }
descending =
    PouchDb.descending


since : Int -> { options | since : Maybe Since } -> { options | since : Maybe Since }
since time options =
    { options | since = Just (Since time) }


sinceNow : { options | since : Maybe Since } -> { options | since : Maybe Since }
sinceNow options =
    { options | since = Just Now }


limit : Int -> { options | limit : Maybe Int } -> { options | limit : Maybe Int }
limit =
    PouchDb.limit


timeout : Int -> { options | timeout : Maybe Int } -> { options | timeout : Maybe Int }
timeout time options =
    { options | timeout = Just time }
