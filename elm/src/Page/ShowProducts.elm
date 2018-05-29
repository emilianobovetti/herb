module Page.ShowProducts exposing (main)

import Model.Product as Product exposing (Product)
import Json.Decode as Decode exposing (Value, decodeValue)
import Html.Attributes as Attributes exposing (class, for, value, type_)
import Html exposing (Html, text, td)
import Html.Events exposing (onClick)
import RequestHandler exposing (RequestHandler)
import Lang exposing (Display, idToString)
import Maybe exposing (withDefault)
import PouchDb exposing (PouchDb)
import Rocket exposing ((=>))
import Input exposing (Input)
import Dict exposing (Dict)
import Time exposing (Time)
import Model.Stock as Stock
import Model.Sale as Sale
import Template.Display
import PouchDb.AllDocs
import PouchDb.Changes
import PouchDb.Query
import PouchDb.Put
import PatternSearch
import Result.Extra
import ModelHelper
import Validation
import List.Extra
import BarCode
import Task
import Dom


type Msg
    = Ignore
    | Toggle Column
    | DbError Value
    | ShowAllProducts
    | ProductsChange Value
    | SearchQuery Input
    | AllProducts Value
    | ProductQuantities Value
    | ClickedProduct String
    | CancelProductUpdate
    | UpdateProductRev Value
    | ProductUpdateSubmit Input
    | UpdateProductQuantity Product Int Time
    | PortMsg (RequestHandler Msg) Msg


type alias Model =
    { localDb : PouchDb
    , display : Maybe Display
    , columns : List Column
    , searchQuery : Maybe String
    , filteredProducts : List Product
    , allProducts : List Product
    , productQuantities : Maybe (Dict String Int)
    , productOnEdit : Maybe Product
    , requestHandler : RequestHandler Msg
    }


type alias Flags =
    { localDb : PouchDb
    , environment : String
    }


type Column
    = Code
    | Producer
    | Distributor
    | Line
    | Name
    | Quantity


focusCmd : Cmd Msg
focusCmd =
    Dom.focus "search-query"
        |> Task.attempt (\_ -> Ignore)


init : Flags -> ( Model, Cmd Msg )
init { localDb } =
    { localDb = localDb
    , display = Nothing
    , columns = [ Producer, Distributor, Line, Name, Quantity ]
    , searchQuery = Nothing
    , filteredProducts = []
    , allProducts = []
    , productQuantities = Nothing
    , productOnEdit = Nothing
    , requestHandler = RequestHandler.init Ignore
    }
        |> allProductsRequest
        |> listenToDbChanges
        |> searchFormHandler
        |> productsFormHandler
        |> flip ModelHelper.addCmd focusCmd
        |> ModelHelper.update


allProductsRequest : Model -> Model
allProductsRequest model =
    PouchDb.defaultQueryOptions
        |> PouchDb.includeDocs
        |> PouchDb.startEnd Product.idPrefix (Product.idPrefix ++ "\xFFFF")
        |> PouchDb.AllDocs.request (Result.Extra.unpack DbError AllProducts) model.localDb
        |> ModelHelper.addPortCmd model


listenToDbChanges : Model -> Model
listenToDbChanges model =
    PouchDb.Changes.defaults
        |> PouchDb.Changes.live
        |> PouchDb.Changes.includeDocs
        |> PouchDb.Changes.sinceNow
        |> PouchDb.Changes.request (Result.Extra.unpack DbError ProductsChange) model.localDb
        |> ModelHelper.addPortCmd model


searchFormHandler : Model -> Model
searchFormHandler model =
    Input.onSubmit SearchQuery "search-form"
        |> ModelHelper.addPortCmd model


productsFormHandler : Model -> Model
productsFormHandler model =
    Input.onSubmit ProductUpdateSubmit "products-form"
        |> ModelHelper.addPortCmd model


unsetProductOnEdit : Model -> Model
unsetProductOnEdit model =
    { model | productOnEdit = Nothing }


sortProducts : List Product -> List Product
sortProducts products =
    let
        serialize : Product -> String
        serialize product =
            String.toLower product.producer ++ String.toLower product.name
    in
        List.sortBy serialize products


decodeAllProducts : Value -> Model -> Model
decodeAllProducts json model =
    case decodeValue (PouchDb.AllDocs.decoder Product.decoder) json of
        Ok products ->
            { model | allProducts = sortProducts products }

        Err error ->
            { model | display = Just (Lang.jsonDecodeError error) }


filterProducts : Model -> Maybe String -> Model
filterProducts ({ allProducts } as model) maybeInput =
    case maybeInput of
        Just input ->
            let
                lowerInput : String
                lowerInput =
                    String.toLower input

                barCodeNormalized : String
                barCodeNormalized =
                    BarCode.normalize input

                barCodeMatchedProducts : List Product
                barCodeMatchedProducts =
                    List.filter (.code >> String.startsWith barCodeNormalized) allProducts

                tokenizer : String -> List String
                tokenizer str =
                    String.split " " str
                        |> List.concatMap (String.split "'")
                        |> List.concatMap (String.split "-")
                        |> List.filter ((/=) "")

                stringifier : Product -> String
                stringifier product =
                    String.toLower product.name
                        ++ " "
                        ++ String.toLower product.producer
                        ++ " "
                        ++ String.toLower (withDefault "" product.distributor)
                        ++ " "
                        ++ String.toLower (withDefault "" product.line)

                threshold : PatternSearch.Threshold
                threshold =
                    case List.length (tokenizer lowerInput) of
                        0 ->
                            { maxDistance = 0, minMatches = 0 }

                        1 ->
                            { maxDistance = 1, minMatches = 1 }

                        2 ->
                            { maxDistance = 1, minMatches = 2 }

                        3 ->
                            { maxDistance = 1, minMatches = 3 }

                        _ ->
                            { maxDistance = 2, minMatches = 4 }

                products : List Product
                products =
                    if barCodeMatchedProducts /= [] then
                        barCodeMatchedProducts
                    else
                        PatternSearch.filterList tokenizer stringifier threshold lowerInput allProducts
                            |> List.map .element
            in
                { model | filteredProducts = sortProducts products }

        Nothing ->
            { model | filteredProducts = [] }


productQuantitiesRequest : Model -> Model
productQuantitiesRequest model =
    PouchDb.defaultQueryOptions
        |> PouchDb.group
        |> PouchDb.Query.request
            (Result.Extra.unpack DbError ProductQuantities)
            model.localDb
            "product_aggregation/quantity"
        |> ModelHelper.addPortCmd model


decodeProductQuantities : Value -> Model -> Model
decodeProductQuantities json model =
    let
        decoder : Decode.Decoder (List ( String, Int ))
        decoder =
            Decode.map2 (,)
                (Decode.field "key" Decode.string)
                (Decode.field "value" Decode.int)
                |> Decode.list
    in
        case decodeValue (Decode.field "rows" decoder) json of
            Ok list ->
                { model | productQuantities = Just (Dict.fromList list) }

            Err error ->
                { model | display = Just (Lang.jsonDecodeError error) }


validateQuantity : Input -> Result Validation.Errors Int
validateQuantity input =
    Input.validate input
        |> Input.with "quantity" Validation.int
        |> Input.end
        |> Result.map (Validation.getInt "quantity")


updateProductQuantityRequest : Product -> Input -> Model -> Model
updateProductQuantityRequest product input model =
    case ( model.productQuantities, validateQuantity input ) of
        ( Just productQuantities, Ok newQuantity ) ->
            let
                oldQuantity : Int
                oldQuantity =
                    Dict.get product.code productQuantities
                        |> withDefault 0

                quantityDiff : Int
                quantityDiff =
                    newQuantity - oldQuantity
            in
                Task.perform (UpdateProductQuantity product quantityDiff) Time.now
                    |> ModelHelper.addCmd model
                    |> ModelHelper.updateDisplay Nothing

        ( _, Err errors ) ->
            if Dict.get "quantity" errors /= Just Validation.Missing then
                { model | display = Just (Lang.displayErrors errors) }
            else
                ModelHelper.updateDisplay Nothing model

        ( Nothing, _ ) ->
            { model | display = Just (Lang.Error Lang.wait) }


updateProductQuantity : Product -> Int -> Time -> Model -> Model
updateProductQuantity { code } quantityDiff now ({ localDb } as model) =
    let
        json : Maybe Value
        json =
            if quantityDiff > 0 then
                Stock.create code now quantityDiff
                    |> Stock.toJson
                    |> Just
            else if quantityDiff < 0 then
                Sale.create code now -quantityDiff
                    |> Sale.toJson
                    |> Just
            else
                Nothing

        put : Value -> RequestHandler Msg
        put json =
            json
                |> PouchDb.Put.request (Result.Extra.unpack DbError (\_ -> Ignore)) localDb
                |> RequestHandler.addPortCmd model.requestHandler

        updateQuantity : Maybe Int -> Maybe Int
        updateQuantity quantity =
            withDefault 0 quantity
                |> (+) quantityDiff
                |> Just
    in
        { model
            | productQuantities =
                Maybe.map (Dict.update code updateQuantity) model.productQuantities
            , requestHandler =
                Maybe.map put json
                    |> withDefault model.requestHandler
        }


updateProduct : Input -> Model -> Model
updateProduct input model =
    case ( model.productOnEdit, Product.validate input ) of
        ( Just product, Ok new ) ->
            let
                product_ =
                    { product
                        | name = new.name
                        , producer = new.producer
                        , distributor = new.distributor
                        , line = new.line
                    }

                model_ =
                    if product /= product_ then
                        Product.toJson product_
                            |> PouchDb.Put.request (Result.Extra.unpack DbError UpdateProductRev) model.localDb
                            |> ModelHelper.addPortCmd model
                    else
                        model
            in
                ModelHelper.updateDisplay Nothing model_
                    |> updateProductQuantityRequest product_ input
                    |> unsetProductOnEdit

        ( Nothing, Ok _ ) ->
            { model | display = Nothing }

        ( _, Err errors ) ->
            { model | display = Just (Lang.displayErrors errors) }


getProductByCode : Model -> String -> Maybe Product
getProductByCode { allProducts } code =
    case List.filter (.code >> (==) code) allProducts of
        product :: _ ->
            Just product

        [] ->
            Nothing


decodeChanges : Value -> Model -> Model
decodeChanges json model =
    case decodeValue (PouchDb.Changes.decoder Product.decoder) json of
        Ok product ->
            let
                replaceIn : List Product -> List Product
                replaceIn list =
                    List.Extra.replaceIf (.id >> (==) product.id) product list
            in
                { model
                    | allProducts = replaceIn model.allProducts
                    , filteredProducts = replaceIn model.filteredProducts
                }

        Err _ ->
            model


newRevDecoder : Decode.Decoder { id : String, rev : String }
newRevDecoder =
    Decode.map2 (\id rev -> { id = id, rev = rev })
        (Decode.field "id" Decode.string)
        (Decode.field "rev" Decode.string)


updateProductRev : Value -> Model -> Model
updateProductRev json model =
    case decodeValue newRevDecoder json of
        Ok { id, rev } ->
            let
                revUpdater : List Product -> List Product
                revUpdater products =
                    List.Extra.updateIf (.id >> (==) id) (\p -> { p | rev = Just rev }) products
            in
                { model | allProducts = revUpdater model.allProducts }

        Err error ->
            { model | display = Just (Lang.jsonDecodeError error) }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Ignore ->
            model ! []

        SearchQuery input ->
            let
                query : Maybe String
                query =
                    Input.getValue "search-query" input
                        |> Maybe.map String.trim
            in
                if query == Just "" then
                    { model | searchQuery = Nothing } ! []
                else
                    filterProducts { model | searchQuery = query } query ! []

        Toggle column ->
            if List.member column model.columns then
                { model | columns = List.Extra.remove column model.columns } ! []
            else
                { model | columns = column :: model.columns } ! []

        DbError error ->
            { model | display = Just (Lang.databaseError error) } ! []

        AllProducts json ->
            decodeAllProducts json model
                |> productQuantitiesRequest
                |> ModelHelper.update

        ShowAllProducts ->
            { model | searchQuery = Nothing, filteredProducts = [] } ! []

        ProductQuantities json ->
            decodeProductQuantities json model ! []

        ClickedProduct code ->
            { model | productOnEdit = getProductByCode model code } ! []

        CancelProductUpdate ->
            unsetProductOnEdit model ! []

        ProductUpdateSubmit input ->
            updateProduct input model
                |> ModelHelper.update

        ProductsChange json ->
            decodeChanges json model
                |> ModelHelper.update

        UpdateProductRev json ->
            updateProductRev json model ! []

        UpdateProductQuantity product quantityDiff now ->
            updateProductQuantity product quantityDiff now model
                |> ModelHelper.update

        PortMsg requestHandler portMsg ->
            update portMsg { model | requestHandler = requestHandler }


columnToString : Column -> String
columnToString column =
    case column of
        Code ->
            "code"

        Producer ->
            "producer"

        Distributor ->
            "distributor"

        Line ->
            "line"

        Name ->
            "name"

        Quantity ->
            "quantity"


viewProductEditTd : Model -> Column -> String -> Html Msg
viewProductEditTd { columns } column value_ =
    td [ Attributes.classList [ "hidden" => not (List.member column columns) ] ]
        [ Html.input
            [ Attributes.id (columnToString column)
            , class "pure-input-1"
            , type_ "text"
            , value value_
            ]
            []
        ]


viewProduct : Model -> Product -> List (Html Msg)
viewProduct ({ columns, productOnEdit } as model) product =
    let
        quantity : Maybe String
        quantity =
            model.productQuantities
                |> Maybe.withDefault Dict.empty
                |> Dict.get product.code
                |> Maybe.map toString

        productCodeOnEdit : Maybe String
        productCodeOnEdit =
            Maybe.map .code productOnEdit

        viewCol : Column -> String -> Html Msg
        viewCol column content =
            if List.member column columns then
                td [] [ text content ]
            else
                Html.span [ class "hidden" ] []

        colNum : Int
        colNum =
            List.length model.columns

        editTd : Column -> String -> Html Msg
        editTd =
            viewProductEditTd model
    in
        if productCodeOnEdit == Just product.code then
            [ Html.tr [ class "active" ]
                [ Html.input [ Attributes.id "code", type_ "hidden", value product.code ] []
                , viewCol Code product.code
                , editTd Producer product.producer
                , editTd Distributor (withDefault "" product.distributor)
                , editTd Line (withDefault "" product.line)
                , editTd Name product.name
                , editTd Quantity (withDefault "" quantity)
                ]
            , Html.tr [ class "pure-control-group" ]
                [ Html.td [ Attributes.colspan colNum ]
                    [ Html.button
                        [ type_ "submit", class "pure-button" ]
                        [ text Lang.update ]
                    , Html.button
                        [ onClick CancelProductUpdate, class "pure-button" ]
                        [ text Lang.cancel ]
                    ]
                ]
            ]
        else
            [ Html.tr [ onClick (ClickedProduct product.code) ]
                [ viewCol Code product.code
                , viewCol Producer product.producer
                , viewCol Distributor (withDefault "∅" product.distributor)
                , viewCol Line (withDefault "∅" product.line)
                , viewCol Name product.name
                , viewCol Quantity (withDefault "∅" quantity)
                ]
            ]


viewProductList : Model -> List Product -> Html Msg
viewProductList ({ columns } as model) products =
    let
        viewCol : Column -> Html Msg
        viewCol column =
            if List.member column columns then
                td [] [ columnToString column |> idToString |> text ]
            else
                Html.span [ class "hidden" ] []
    in
        Html.form [ Attributes.id "products-form", class "pure-form" ]
            [ Html.table [ class "pure-table pure-table-bordered" ]
                [ Html.thead []
                    [ Html.tr []
                        [ viewCol Code
                        , viewCol Producer
                        , viewCol Distributor
                        , viewCol Line
                        , viewCol Name
                        , viewCol Quantity
                        ]
                    ]
                , Html.tbody [] (List.concatMap (viewProduct model) products)
                ]
            ]


viewToggleColumn : Model -> Column -> List (Html Msg)
viewToggleColumn { columns } column =
    let
        columnName : String
        columnName =
            columnToString column

        fieldId : String
        fieldId =
            "toggle-" ++ columnName

        localColumnName : String
        localColumnName =
            Lang.idToString columnName

        member : Bool
        member =
            List.member column columns

        disabled : Html.Attribute Msg
        disabled =
            Attributes.disabled
                (member && List.length columns < 3)

        checked : Html.Attribute Msg
        checked =
            Attributes.checked member
    in
        [ Html.input
            [ onClick (Toggle column)
            , Attributes.id fieldId
            , type_ "checkbox"
            , disabled
            , checked
            ]
            []
        , Html.label [ class "checkmark", for fieldId ] [ text localColumnName ]
        ]


view : Model -> Html Msg
view ({ allProducts, searchQuery, filteredProducts } as model) =
    let
        products : List Product
        products =
            if filteredProducts == [] then
                allProducts
            else
                filteredProducts

        message : String
        message =
            if searchQuery /= Nothing && filteredProducts == [] then
                Lang.noMatch
            else if filteredProducts == [] then
                Lang.allProducts
            else
                ""

        toggle : Column -> List (Html Msg)
        toggle =
            viewToggleColumn model
    in
        Html.div []
            [ Template.Display.message model.display
            , Html.form [ Attributes.id "search-form", class "pure-form" ]
                [ Html.input
                    [ Attributes.id "search-query"
                    , class "display-block pure-u-2-3"
                    , type_ "text"
                    , value (withDefault "" searchQuery)
                    ]
                    []
                , Html.div [ class "pure-controls" ]
                    [ Html.button
                        [ type_ "submit", class "float-left pure-button pure-button-primary" ]
                        [ text Lang.search ]
                    ]
                ]
            , Html.button [ onClick ShowAllProducts, class "pure-button pure-button-primary" ]
                [ text Lang.showAll ]
            , Html.p [] [ text message ]
            , Html.form [ class "pure-form" ] <|
                List.concat
                    [ toggle Code
                    , toggle Producer
                    , toggle Distributor
                    , toggle Line
                    , toggle Name
                    , toggle Quantity
                    ]
            , viewProductList model products
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
