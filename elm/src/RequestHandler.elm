port module RequestHandler
    exposing
        ( RequestHandler
        , init
        , addCmd
        , addPortCmd
        , cmd
        , sub
        )

import Json.Encode as Encode exposing (Value)
import PortCmd exposing (PortCmd)
import Dict exposing (Dict)


type alias Id =
    Int


type alias RequestHandler msg =
    { subscriptions : Dict Id (PortCmd msg)
    , commands : List (Cmd msg)
    , onMissingHandler : msg
    , counter : Id
    }


type alias OutgoingDataExchange =
    { id : Id
    , message : Value
    }


type alias IncomingDataExchange =
    { id : Id
    , ok : Maybe Value
    , err : Maybe Value
    }


port outgoing : OutgoingDataExchange -> Cmd msg


port incoming : (IncomingDataExchange -> msg) -> Sub msg


init : msg -> RequestHandler msg
init msg =
    { subscriptions = Dict.empty
    , onMissingHandler = msg
    , commands = []
    , counter = 0
    }


addCmd : RequestHandler msg -> Cmd msg -> RequestHandler msg
addCmd requestHandler cmd =
    { requestHandler | commands = cmd :: requestHandler.commands }


addPortCmd : RequestHandler msg -> PortCmd msg -> RequestHandler msg
addPortCmd { subscriptions, onMissingHandler, commands, counter } portCmd =
    { subscriptions = Dict.insert counter portCmd subscriptions
    , commands = outgoing { id = counter, message = PortCmd.msg portCmd } :: commands
    , onMissingHandler = onMissingHandler
    , counter = counter + 1
    }


cmd : RequestHandler msg -> ( RequestHandler msg, Cmd msg )
cmd requestHandler =
    ( { requestHandler | commands = [] }, Cmd.batch requestHandler.commands )


incomingDataToResult : IncomingDataExchange -> Result Value Value
incomingDataToResult { ok, err } =
    case ( ok, err ) of
        ( Nothing, Nothing ) ->
            -- this shouldn't happen
            Err Encode.null

        ( _, Just error ) ->
            Err error

        ( Just val, _ ) ->
            Ok val


type alias Wrapper msg =
    RequestHandler msg -> msg -> msg


eventHandler : RequestHandler msg -> Wrapper msg -> IncomingDataExchange -> msg
eventHandler ({ subscriptions } as requestHandler) wrapper data =
    case Dict.get data.id subscriptions of
        Just (PortCmd.Persistent toMessage _) ->
            incomingDataToResult data
                |> toMessage
                |> wrapper requestHandler

        Just (PortCmd.Disposable toMessage _) ->
            incomingDataToResult data
                |> toMessage
                |> wrapper { requestHandler | subscriptions = Dict.remove data.id subscriptions }

        Nothing ->
            requestHandler.onMissingHandler


sub : RequestHandler msg -> Wrapper msg -> Sub msg
sub requestHandler wrapper =
    incoming (eventHandler requestHandler wrapper)
