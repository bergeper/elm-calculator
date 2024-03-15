module View.Header exposing (viewHeader)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html exposing (b)


viewHeader : Html msg
viewHeader =
    div [ class "header" ]
        [  
            b [] [text "Simple Elm Calculator"]
        ]