module Template.Display exposing (message)

import Lang exposing (Display(Dialog, Error))
import Html.Attributes exposing (class)
import Html exposing (Html, text)


message : Maybe Display -> Html msg
message display =
    case display of
        Just (Error msg) ->
            Html.div [ class "alert alert-danger" ] [ text msg ]

        Just (Dialog msg) ->
            Html.div [ class "alert alert-success" ] [ text msg ]

        Nothing ->
            Html.text ""
