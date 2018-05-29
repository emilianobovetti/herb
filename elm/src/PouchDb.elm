module PouchDb
    exposing
        ( PouchDb
        , Id
        , Rev
        , Attachments
            ( Base64
            , Binary
            )
        , Selection
            ( Keys
            , Start
            , End
            , StartEnd
            )
        , QueryOptions
        , encodeQueryOptions
        , defaultQueryOptions
        , includeDocs
        , conflicts
        , base64
        , binary
        , inclusiveEnd
        , descending
        , skip
        , limit
        , key
        , keys
        , start
        , end
        , startEnd
        , group
        )

import Json.Extra.Encode as Encode exposing (Value)
import Rocket exposing ((=>))


type alias PouchDb =
    Value


type alias Id =
    String


type alias Rev =
    String


type Attachments
    = Base64
    | Binary


type Selection
    = Keys (List Id)
    | Start Id
    | End Id
    | StartEnd Id Id


type alias QueryOptions =
    { includeDocs : Maybe Bool
    , conflicts : Maybe Bool
    , attachments : Maybe Attachments
    , inclusiveEnd : Maybe Bool
    , descending : Maybe Bool
    , skip : Maybe Int
    , limit : Maybe Int
    , key : Maybe Id
    , selection : Maybe Selection
    , group : Maybe Bool
    }


encodeEndkey : QueryOptions -> Value
encodeEndkey { selection } =
    case selection of
        Just (End id) ->
            Encode.string id

        Just (StartEnd _ end) ->
            Encode.string end

        _ ->
            Encode.null


encodeStartkey : QueryOptions -> Value
encodeStartkey { selection } =
    case selection of
        Just (Start id) ->
            Encode.string id

        Just (StartEnd start _) ->
            Encode.string start

        _ ->
            Encode.null


encodeKeys : QueryOptions -> Value
encodeKeys { selection } =
    case selection of
        Just (Keys lst) ->
            List.map Encode.string lst |> Encode.list

        _ ->
            Encode.null


encodeQueryOptions : QueryOptions -> Value
encodeQueryOptions options =
    Encode.object
        [ "include_docs" => Encode.maybeBool options.includeDocs
        , "conflicts" => Encode.maybeBool options.conflicts
        , "attachments" => Encode.bool (options.attachments /= Nothing)
        , "binary" => Encode.bool (options.attachments == Just Binary)
        , "inclusive_end" => Encode.maybeBool options.inclusiveEnd
        , "descending" => Encode.maybeBool options.descending
        , "skip" => Encode.maybeInt options.skip
        , "limit" => Encode.maybeInt options.limit
        , "key" => Encode.maybeString options.key
        , "endkey" => encodeEndkey options
        , "startkey" => encodeStartkey options
        , "keys" => encodeKeys options
        , "group" => Encode.maybeBool options.group
        ]


defaultQueryOptions : QueryOptions
defaultQueryOptions =
    { includeDocs = Nothing
    , conflicts = Nothing
    , attachments = Nothing
    , inclusiveEnd = Nothing
    , descending = Nothing
    , skip = Nothing
    , limit = Nothing
    , key = Nothing
    , selection = Nothing
    , group = Nothing
    }


includeDocs : { options | includeDocs : Maybe Bool } -> { options | includeDocs : Maybe Bool }
includeDocs options =
    { options | includeDocs = Just True }


conflicts : { options | conflicts : Maybe Bool } -> { options | conflicts : Maybe Bool }
conflicts options =
    { options | conflicts = Just True }


base64 : { options | attachments : Maybe Attachments } -> { options | attachments : Maybe Attachments }
base64 options =
    { options | attachments = Just Base64 }


binary : { options | attachments : Maybe Attachments } -> { options | attachments : Maybe Attachments }
binary options =
    { options | attachments = Just Binary }


inclusiveEnd : { options | inclusiveEnd : Maybe Bool } -> { options | inclusiveEnd : Maybe Bool }
inclusiveEnd options =
    { options | inclusiveEnd = Just True }


descending : { options | descending : Maybe Bool } -> { options | descending : Maybe Bool }
descending options =
    { options | descending = Just True }


skip : Int -> { options | skip : Maybe Int } -> { options | skip : Maybe Int }
skip n options =
    { options | skip = Just n }


limit : Int -> { options | limit : Maybe Int } -> { options | limit : Maybe Int }
limit n options =
    { options | limit = Just n }


key : Id -> { options | key : Maybe Id } -> { options | key : Maybe Id }
key id options =
    { options | key = Just id }


keys : List Id -> { options | selection : Maybe Selection } -> { options | selection : Maybe Selection }
keys ids options =
    { options | selection = Just (Keys ids) }


start : Id -> { options | selection : Maybe Selection } -> { options | selection : Maybe Selection }
start id options =
    { options | selection = Just (Start id) }


end : Id -> { options | selection : Maybe Selection } -> { options | selection : Maybe Selection }
end id options =
    { options | selection = Just (End id) }


startEnd : Id -> Id -> { options | selection : Maybe Selection } -> { options | selection : Maybe Selection }
startEnd idStart idEnd options =
    { options | selection = Just (StartEnd idStart idEnd) }


group : { options | group : Maybe Bool } -> { options | group : Maybe Bool }
group options =
    { options | group = Just True }
