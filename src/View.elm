module View exposing (..)

import Html exposing (Html, b, div, main_, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Model exposing (Model, Operation(..))
import Update exposing (Msg(..))


view : Model -> Html Msg
view model =
    let
        operatorView func op =
            div [ class "calculator__pad--operators", onClick (Operator func) ] [ text op ]

        numView num =
            div [ class "calculator__pad--number", onClick (Number num) ] [ text num ]

        choiceView func str =
            div [ class "calculator__pad--choice", onClick func ] [ text str ]

        row html =
            div [ class "calculator__padRow" ] html

        zeroView =
            div [ class "calculator__pad--zero", onClick (Number "0") ] [ text "0" ]

        equalsView =
            div [ class "calculator__pad--equals", onClick Equals ] [ text "=" ]
    in
    div []
        [ div [ class "header" ]
            [ b [] [ text "Simple Elm Calculator" ]
            ]
        , main_ [ class "container" ]
            [ div [ class "program" ]
                [ div [ class "calculator__input" ]
                    [ span [ class "calculator__errorMsg" ] [ text model.errorMsg ]
                    , span [ class "calculator__valueOne" ] [ text model.valueOne ]
                    , div []
                        [ span [ class "calculator__valueTwo" ] [ text model.operator ]
                        , span [ class "calculator__valueTwo" ] [ text model.valueTwo ]
                        ]
                    ]
                , div [ class "calculator" ]
                    [ row
                        [ choiceView Clear "C"
                        , choiceView Backspace "<"
                        , operatorView Divide "÷"
                        ]
                    , row
                        [ numView "7"
                        , numView "8"
                        , numView "9"
                        , operatorView Multiply "×"
                        ]
                    , row
                        [ numView "4"
                        , numView "5"
                        , numView "6"
                        , operatorView Subtract "-"
                        ]
                    , row
                        [ numView "1"
                        , numView "2"
                        , numView "3"
                        , operatorView Add "+"
                        ]
                    , row
                        [ zeroView
                        , equalsView
                        ]
                    ]
                ]
            ]
        ]
