module Lang
    exposing
        ( Display(Error, Dialog)
        , insert
        , update
        , cancel
        , search
        , show
        , hide
        , noMatch
        , back
        , wait
        , showAll
        , allProducts
        , databaseError
        , jsonDecodeError
        , databaseSuccessfulPut
        , idToString
        , inputErrorToString
        , displayErrors
        )

import Validation exposing (TypeError)
import Json.Decode exposing (Value)
import PouchDb.Error
import Dict


type Display
    = Error String
    | Dialog String


insert : String
insert =
    "inserisci"


update : String
update =
    "aggiorna"


cancel : String
cancel =
    "annulla"


search : String
search =
    "cerca"


show : String
show =
    "mostra"


hide : String
hide =
    "nascondi"


noMatch : String
noMatch =
    "nessun risultato"


back : String
back =
    "indietro"


wait : String
wait =
    "attendere"


showAll : String
showAll =
    "mostra tutti"


allProducts : String
allProducts =
    "tutti i prodotti"


databaseError : Value -> Display
databaseError json =
    let
        unableToDecode : PouchDb.Error.Error
        unableToDecode =
            { status = -1
            , name = PouchDb.Error.Unknown
            , message = "Unable to decode database error, dump:" ++ toString json
            }

        error : PouchDb.Error.Error
        error =
            Json.Decode.decodeValue PouchDb.Error.decoder json
                |> Result.withDefault unableToDecode
    in
        Error (error.message ++ ", code: " ++ toString error.status)


jsonDecodeError : String -> Display
jsonDecodeError error =
    Error ("Unable to decode json: " ++ error)


databaseSuccessfulPut : Display
databaseSuccessfulPut =
    Dialog "Inserimento completato"


idToString : String -> String
idToString name =
    case name of
        "name" ->
            "nome"

        "code" ->
            "codice"

        "producer" ->
            "produttore"

        "distributor" ->
            "distributore"

        "line" ->
            "linea"

        "format" ->
            "formato"

        "listPrice" ->
            "prezzo di listino"

        "description" ->
            "descrizione"

        "labelCode" ->
            "codice etichetta"

        "productCode" ->
            "codice prodotto"

        "category" ->
            "categoria"

        "color" ->
            "colore"

        "vegan" ->
            "vegan"

        "date" ->
            "data"

        "items" ->
            "articoli"

        "quantity" ->
            "quantità"

        _ ->
            name


inputErrorToString : String -> TypeError -> String
inputErrorToString field error =
    let
        fieldName : String
        fieldName =
            idToString field
    in
        case error of
            Validation.Missing ->
                "Il campo " ++ fieldName ++ " è obbligatorio"

            Validation.Negative ->
                "Il campo " ++ fieldName ++ " deve contenere un valore maggiore o uguale a 0"

            Validation.NonPositive ->
                "Il campo " ++ fieldName ++ " deve contenere un valore positivo"

            Validation.InvalidString ->
                "Il formato del campo " ++ fieldName ++ " non è valido"

            Validation.InvalidFloat ->
                "Il campo " ++ fieldName ++ " deve contenere un numero"

            Validation.InvalidBool ->
                "Il campo " ++ fieldName ++ " non contiene un valore valido"

            Validation.InvalidDate ->
                "Il campo " ++ fieldName ++ " deve contenere una data valida"

            Validation.InvalidInt ->
                "Il campo " ++ fieldName ++ " deve contentere un numero intero"


displayErrors : Validation.Errors -> Display
displayErrors errors =
    Dict.toList errors
        |> List.map (\( key, err ) -> inputErrorToString key err)
        |> List.foldl (\str acc -> acc ++ str ++ "\n") ""
        |> String.trim
        |> Error
