module PouchDb.Error
    exposing
        ( Name
            ( BadArg
            , Conflict
            , NotFound
            , Forbidden
            , MissingId
            , BadRequest
            , FileExists
            , InvalidUrl
            , MissingStub
            , Unauthorized
            , DocValidation
            , WebSqlWentBad
            , InvalidRequest
            , LevelDbWentBad
            , QueryParseError
            , IndexedDbWentBad
            , PreconditionFailed
            , Unknown
            )
        , Error
        , decoder
        )

import Json.Decode.Pipeline exposing (decode, required)
import Json.Decode as Decode exposing (Decoder)


type Name
    = BadArg
    | Conflict
    | NotFound
    | Forbidden
    | MissingId
    | BadRequest
    | FileExists
    | InvalidUrl
    | MissingStub
    | Unauthorized
    | DocValidation
    | WebSqlWentBad
    | InvalidRequest
    | LevelDbWentBad
    | QueryParseError
    | IndexedDbWentBad
    | PreconditionFailed
    | Unknown


type alias Error =
    { status : Int
    , name : Name
    , message : String
    }


decoder : Decoder Error
decoder =
    decode Error
        |> required "status" Decode.int
        |> required "name" nameDecoder
        |> required "message" Decode.string


nameDecoder : Decoder Name
nameDecoder =
    Decode.map nameFromString Decode.string


nameFromString : String -> Name
nameFromString str =
    case str of
        "badarg" ->
            BadArg

        "conflict" ->
            Conflict

        "not_found" ->
            NotFound

        "forbidden" ->
            Forbidden

        "missing_id" ->
            MissingId

        "bad_request" ->
            BadRequest

        "file_exists" ->
            FileExists

        "invalid_url" ->
            InvalidUrl

        "missing_stub" ->
            MissingStub

        "unauthorized" ->
            Unauthorized

        "doc_validation" ->
            DocValidation

        "web_sql_went_bad" ->
            WebSqlWentBad

        "invalid_request" ->
            InvalidRequest

        "levelDB_went_went_bad" ->
            LevelDbWentBad

        "query_parse_error" ->
            QueryParseError

        "indexed_db_went_bad" ->
            IndexedDbWentBad

        "precondition_failed" ->
            PreconditionFailed

        "unknown_error" ->
            Unknown

        _ ->
            Unknown
