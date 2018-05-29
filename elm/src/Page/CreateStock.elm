module Page.CreateStock exposing (main)

import Html exposing (Html, div, button, text, form)
import Html.Attributes exposing (id, class, type_)
import RequestHandler exposing (RequestHandler)
import Json.Encode exposing (Value)
import PouchDb exposing (PouchDb)
import Lang exposing (Display)
import Input exposing (Input)
import Time exposing (Time)
import Model.Stock as Stock
import Template.Display
import Template.Input
import Result.Extra
import PouchDb.Put
import ModelHelper
import BarCode
import Task
import Dom


type Msg
    = Ignore
    | NewTime Time
    | DbError Value
    | OnSubmit Input
    | StockCreated Value
    | PortMsg (RequestHandler Msg) Msg


type alias Model =
    { input : Input
    , localDb : PouchDb
    , display : Maybe Display
    , requestHandler : RequestHandler Msg
    }


type alias Flags =
    { localDb : PouchDb
    , environment : String
    }


focusCmd : Cmd Msg
focusCmd =
    Dom.focus "productCode"
        |> Task.attempt (\_ -> Ignore)


init : Flags -> ( Model, Cmd Msg )
init flags =
    { input = Input.empty
    , localDb = flags.localDb
    , display = Nothing
    , requestHandler = RequestHandler.init Ignore
    }
        |> registerCreateStockHandler
        |> flip ModelHelper.addCmd focusCmd
        |> ModelHelper.update


registerCreateStockHandler : Model -> Model
registerCreateStockHandler model =
    Input.onSubmit OnSubmit "create-stock"
        |> ModelHelper.addPortCmd model


createStockRequest : Time -> Model -> Model
createStockRequest now model =
    case Stock.validate now model.input of
        Err errors ->
            { model | input = Input.setValidationErrors errors model.input }

        Ok stock ->
            Stock.toJson stock
                |> PouchDb.Put.request (Result.Extra.unpack DbError StockCreated) model.localDb
                |> ModelHelper.addPortCmd model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Ignore ->
            model ! []

        OnSubmit input ->
            { model | input = Input.mapValue "productCode" BarCode.normalize input }
                ! [ Task.perform NewTime Time.now ]

        NewTime now ->
            createStockRequest now model
                |> ModelHelper.update

        DbError error ->
            { model | display = Just (Lang.databaseError error) } ! []

        StockCreated _ ->
            { model
                | input = Input.empty
                , display = Just Lang.databaseSuccessfulPut
            }
                ! []

        PortMsg requestHandler portMsg ->
            update portMsg { model | requestHandler = requestHandler }


inputTemplate : Input -> String -> String -> Html Msg
inputTemplate input id_ inputType =
    Template.Input.view input
        { id = id_
        , containerClass = "pure-u-1-3"
        , errorClass = "error-message"
        , type_ = inputType
        }


view : Model -> Html Msg
view { input, display } =
    Html.div []
        [ Template.Display.message display
        , form [ id "create-stock", class "pure-form" ]
            [ inputTemplate input "productCode" "text"
            , inputTemplate input "quantity" "number"
            , div [ class "pure-controls" ]
                [ button [ type_ "submit", class "pure-button pure-button-primary" ]
                    [ text Lang.insert ]
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions app =
    RequestHandler.sub app.requestHandler PortMsg


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
