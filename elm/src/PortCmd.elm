module PortCmd exposing (ToMessage, PortCmd(Persistent, Disposable), msg, toMessage)

import Json.Encode exposing (Value)


type alias ToMessage msg =
    Result Value Value -> msg


type PortCmd msg
    = Persistent (ToMessage msg) Value
    | Disposable (ToMessage msg) Value


msg : PortCmd msg -> Value
msg portCmd =
    case portCmd of
        Persistent _ value ->
            value

        Disposable _ value ->
            value


toMessage : PortCmd msg -> ToMessage msg
toMessage portCmd =
    case portCmd of
        Persistent toMsg _ ->
            toMsg

        Disposable toMsg _ ->
            toMsg
