module Page.CreateProduct exposing (main)

import Html exposing (Html, div, button, text, form, fieldset)
import Html.Attributes exposing (id, class, type_)
import RequestHandler exposing (RequestHandler)
import Json.Encode exposing (Value)
import PouchDb exposing (PouchDb)
import Model.Product as Product
import Lang exposing (Display)
import Input exposing (Input)
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
    | OnSubmit Input
    | DbError Value
    | ProductCreated Value
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
    Dom.focus "code"
        |> Task.attempt (\_ -> Ignore)


init : Flags -> ( Model, Cmd Msg )
init flags =
    { input = Input.empty
    , localDb = flags.localDb
    , display = Nothing
    , requestHandler = RequestHandler.init Ignore
    }
        |> registerCreateProductHandler
        |> flip ModelHelper.addCmd focusCmd
        |> ModelHelper.update


registerCreateProductHandler : Model -> Model
registerCreateProductHandler model =
    Input.onSubmit OnSubmit "create-product"
        |> ModelHelper.addPortCmd model


createProductRequest : Model -> Model
createProductRequest model =
    case Product.validate model.input of
        Err errors ->
            { model | input = Input.setValidationErrors errors model.input }

        Ok product ->
            Product.toJson product
                |> PouchDb.Put.request (Result.Extra.unpack DbError ProductCreated) model.localDb
                |> ModelHelper.addPortCmd model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Ignore ->
            model ! []

        OnSubmit input ->
            { model | input = Input.mapValue "code" BarCode.normalize input }
                |> createProductRequest
                |> ModelHelper.update

        DbError error ->
            { model | display = Just (Lang.databaseError error) } ! []

        ProductCreated _ ->
            { model
                | display = Just Lang.databaseSuccessfulPut
                , input = Input.remove "code" (Input.remove "name" model.input)
            }
                ! []

        PortMsg requestHandler portMsg ->
            update portMsg { model | requestHandler = requestHandler }


inputTemplate : Input -> String -> Html Msg
inputTemplate input id_ =
    Template.Input.view input
        { id = id_
        , containerClass = "pure-u-1-3"
        , errorClass = "error-message"
        , type_ = "text"
        }


view : Model -> Html Msg
view { input, display } =
    Html.div []
        [ Template.Display.message display
        , form [ id "create-product", class "pure-form" ]
            [ fieldset [ class "pure-group" ]
                [ inputTemplate input "code"
                , inputTemplate input "name"
                , inputTemplate input "producer"
                ]

            --, fieldset [ class "pure-group" ]
            --[ inputTemplate input "category"
            --, inputTemplate input "description"
            --            ]
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
