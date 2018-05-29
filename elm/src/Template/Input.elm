module Template.Input exposing (InputParams, view)

import Html.Attributes exposing (id, class, type_, value, for)
import Validation exposing (TypeError)
import Input exposing (Input)
import Html exposing (Html)
import Lang


type alias InputParams =
    { id : String
    , containerClass : String
    , errorClass : String
    , type_ : String
    }


viewLabel : InputParams -> Html msg
viewLabel params =
    Html.label [ for params.id ] [ Html.text (Lang.idToString params.id) ]


viewField : String -> InputParams -> Html msg
viewField inputValue params =
    Html.input
        [ id params.id
        , type_ params.type_
        , value inputValue
        ]
        []


viewError : Maybe TypeError -> InputParams -> List (Html msg)
viewError error params =
    case error of
        Just err ->
            [ Html.div
                [ class params.errorClass ]
                [ Html.text (Lang.inputErrorToString params.id err) ]
            ]

        Nothing ->
            []


viewContainer : String -> Maybe TypeError -> InputParams -> Html msg
viewContainer inputValue error params =
    Html.div [ class params.containerClass ] <|
        viewLabel params
            :: viewField inputValue params
            :: viewError error params


view : Input -> InputParams -> Html msg
view input params =
    let
        inputValue : String
        inputValue =
            Input.getWithDefault "" params.id input

        inputError : Maybe Validation.TypeError
        inputError =
            Input.getError params.id input
    in
        viewContainer inputValue inputError params
