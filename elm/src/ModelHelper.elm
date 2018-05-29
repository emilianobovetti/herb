module ModelHelper
    exposing
        ( addCmd
        , addPortCmd
        , update
        , updateDisplay
        )

import RequestHandler exposing (RequestHandler)
import PortCmd exposing (PortCmd)
import Lang exposing (Display)


addCmd : { model | requestHandler : RequestHandler msg } -> Cmd msg -> { model | requestHandler : RequestHandler msg }
addCmd model cmd =
    { model | requestHandler = RequestHandler.addCmd model.requestHandler cmd }


addPortCmd : { model | requestHandler : RequestHandler msg } -> PortCmd msg -> { model | requestHandler : RequestHandler msg }
addPortCmd model portCmd =
    { model | requestHandler = RequestHandler.addPortCmd model.requestHandler portCmd }


update : { model | requestHandler : RequestHandler msg } -> ( { model | requestHandler : RequestHandler msg }, Cmd msg )
update model =
    let
        ( requestHandler, cmd ) =
            RequestHandler.cmd model.requestHandler
    in
        ( { model | requestHandler = requestHandler }, cmd )


updateDisplay : Maybe Display -> { model | display : Maybe Display } -> { model | display : Maybe Display }
updateDisplay display model =
    { model | display = display }
