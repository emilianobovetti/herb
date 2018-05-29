module Page.ShowSales exposing (main)

import Json.Decode exposing (Value, decodeValue)
import RequestHandler exposing (RequestHandler)
import Lang exposing (Display, idToString)
import Model.Sale as Sale exposing (Sale)
import Html.Attributes exposing (class)
import Html exposing (Html, text)
import PouchDb exposing (PouchDb)
import PouchDb.AllDocs
import Template.Display
import Result.Extra
import ModelHelper


type Msg
    = Ignore
    | DbError Value
    | AllSales Value
    | PortMsg (RequestHandler Msg) Msg


type alias Model =
    { localDb : PouchDb
    , display : Maybe Display
    , sales : List Sale
    , requestHandler : RequestHandler Msg
    }


type alias Flags =
    { localDb : PouchDb
    , environment : String
    }


init : Flags -> ( Model, Cmd Msg )
init { localDb } =
    { localDb = localDb
    , display = Nothing
    , sales = []
    , requestHandler = RequestHandler.init Ignore
    }
        |> allSalesRequest
        |> ModelHelper.update


allSalesRequest : Model -> Model
allSalesRequest model =
    PouchDb.defaultQueryOptions
        |> PouchDb.includeDocs
        |> PouchDb.startEnd Sale.idPrefix (Sale.idPrefix ++ "\xFFFF")
        |> PouchDb.AllDocs.request (Result.Extra.unpack DbError AllSales) model.localDb
        |> ModelHelper.addPortCmd model


decodeSales : Value -> Model -> Model
decodeSales value model =
    case decodeValue (PouchDb.AllDocs.decoder Sale.decoder) value of
        Ok sales ->
            { model | sales = sales }

        Err error ->
            { model | display = Just (Lang.jsonDecodeError error) }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Ignore ->
            model ! []

        DbError error ->
            { model | display = Just (Lang.databaseError error) } ! []

        AllSales value ->
            decodeSales value model ! []

        PortMsg requestHandler portMsg ->
            update portMsg { model | requestHandler = requestHandler }


displaySale : Sale -> Html Msg
displaySale sale =
    Html.tr []
        [ Html.td [] [ text sale.id ]
        , Html.td [] [ text sale.productCode ]
        , Html.td [] [ text (toString sale.timestamp) ]
        , Html.td [] [ text (toString sale.quantity) ]
        ]


view : Model -> Html Msg
view { sales, display } =
    Html.div []
        [ Template.Display.message display
        , Html.table [ class "pure-table pure-table-bordered pure-table-striped" ]
            [ Html.thead []
                [ Html.tr []
                    [ Html.td [] [ text "id" ]
                    , Html.td [] [ text (idToString "productCode") ]
                    , Html.td [] [ text (idToString "timestamp") ]
                    , Html.td [] [ text (idToString "quantity") ]
                    ]
                ]
            , Html.tbody [] (List.map displaySale sales)
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
