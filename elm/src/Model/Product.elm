module Model.Product exposing (Product, idPrefix, validate, toJson, decoder)

import Validation exposing (Validator, maybe, getString, getMaybeString)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Json.Extra.Decode as Decode exposing (Decoder, nullable)
import Json.Extra.Encode as Encode
import Input exposing (Input, with)
import Rocket exposing ((=>))
import PouchDb


type alias Product =
    { id : PouchDb.Id
    , rev : Maybe PouchDb.Rev
    , code : String
    , name : String
    , producer : String
    , distributor : Maybe String
    , line : Maybe String
    }


idPrefix : String
idPrefix =
    "product_"


noUnderscore : Validator
noUnderscore =
    Validation.string
        >> Validation.filterString (not << String.contains "_")


validate : Input -> Result Validation.Errors Product
validate input =
    let
        result : Validation.ValidationResult
        result =
            Input.validate input
                |> with "code" noUnderscore
                |> with "name" Validation.string
                |> with "producer" Validation.string
                |> with "distributor" (maybe Validation.string)
                |> with "line" (maybe Validation.string)
                |> Input.end

        dataToProduct : Validation.Data -> Product
        dataToProduct data =
            { id = idPrefix ++ getString "code" data
            , rev = Nothing
            , code = getString "code" data
            , name = getString "name" data
            , producer = getString "producer" data
            , distributor = getMaybeString "distributor" data
            , line = getMaybeString "line" data
            }
    in
        Result.map dataToProduct result


toJson : Product -> Encode.Value
toJson product =
    Encode.object
        [ "_id" => Encode.string product.id
        , "_rev" => Encode.maybeString product.rev
        , "name" => Encode.string product.name
        , "producer" => Encode.string product.producer
        , "distributor" => Encode.maybeString product.distributor
        , "line" => Encode.maybeString product.line
        ]


create :
    String
    -> String
    -> String
    -> String
    -> Maybe String
    -> Maybe String
    -> Product
create id rev name producer distributor line =
    let
        code : String
        code =
            case String.split "_" id of
                _ :: code_ :: _ ->
                    code_

                _ ->
                    id
    in
        { id = id
        , rev = Just rev
        , code = code
        , name = name
        , producer = producer
        , distributor = distributor
        , line = line
        }


decoder : Decoder Product
decoder =
    decode create
        |> required "_id" Decode.string
        |> required "_rev" Decode.string
        |> required "name" Decode.string
        |> required "producer" Decode.string
        |> optional "distributor" (nullable Decode.string) Nothing
        |> optional "line" (nullable Decode.string) Nothing
