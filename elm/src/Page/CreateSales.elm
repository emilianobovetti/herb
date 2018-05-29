module Page.CreateSales exposing (main)

import Html.Attributes exposing (id, class, value, rows, cols, autofocus)
import Html.Events exposing (keyCode)
import Html exposing (Html, button, text, form)
import RequestHandler exposing (RequestHandler)
import Validation exposing (TypeError(Missing))
import Json.Decode exposing (decodeValue)
import Json.Encode exposing (Value)
import PouchDb exposing (PouchDb)
import Lang exposing (Display)
import Input exposing (Input)
import Time exposing (Time)
import Model.Sale as Sale
import Template.Display
import PouchDb.BulkDocs
import Result.Extra
import ModelHelper
import BarCode
import Task
import Dom


type Msg
    = Ignore
    | OnSubmit Input
    | NewTime Time
    | DbError Value
    | SalesCreated Value
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
    Dom.focus textareaId
        |> Task.attempt (\_ -> Ignore)


init : Flags -> ( Model, Cmd Msg )
init flags =
    { localDb = flags.localDb
    , input = Input.empty
    , display = Nothing
    , requestHandler = RequestHandler.init Ignore
    }
        |> registerCreateSalesHandler
        |> flip ModelHelper.addCmd focusCmd
        |> ModelHelper.update


registerCreateSalesHandler : Model -> Model
registerCreateSalesHandler model =
    Input.onSubmit OnSubmit formId
        |> ModelHelper.addPortCmd model


parseInput : Input -> Input
parseInput input =
    let
        parsedText : String
        parsedText =
            Input.getValue textareaId input
                |> Maybe.withDefault ""
                |> String.split "\n"
                |> List.map BarCode.normalize
                |> List.filter ((/=) "")
                |> flip List.append [ "" ]
                |> String.join "\n"
    in
        Input.setValue textareaId parsedText input


createSalesRequest : Time -> Model -> Model
createSalesRequest now model =
    case Input.getValue textareaId model.input |> Sale.fromString now of
        [] ->
            { model | input = Input.setError textareaId Missing model.input }

        sales ->
            List.map Sale.toJson sales
                |> PouchDb.BulkDocs.request (Result.Extra.unpack DbError SalesCreated) model.localDb
                |> ModelHelper.addPortCmd model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Ignore ->
            model ! []

        OnSubmit input ->
            { model | input = parseInput input } ! [ Task.perform NewTime Time.now ]

        NewTime now ->
            createSalesRequest now model
                |> ModelHelper.update

        SalesCreated _ ->
            { model
                | input = Input.empty
                , display = Just Lang.databaseSuccessfulPut
            }
                ! []

        DbError error ->
            { model | display = Just (Lang.databaseError error) } ! []

        PortMsg requestHandler portMsg ->
            update portMsg { model | requestHandler = requestHandler }


formId : String
formId =
    "create-sales"


textareaId : String
textareaId =
    "codes"


onKeyDown : (Int -> Msg) -> Html.Attribute Msg
onKeyDown tagger =
    Html.Events.on "keydown" (Json.Decode.map tagger keyCode)


view : Model -> Html Msg
view model =
    let
        textAreaDefAtt : List (Html.Attribute Msg)
        textAreaDefAtt =
            [ id textareaId
            , cols 40
            , rows 20
            , value (Input.getValue textareaId model.input |> Maybe.withDefault "")
            ]
    in
        Html.div []
            [ Template.Display.message model.display
            , form [ id formId, class "pure-form" ]
                [ Html.textarea textAreaDefAtt []
                , Html.div [ class "pure-controls" ]
                    [ button [ class "pure-button pure-button-primary" ]
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
