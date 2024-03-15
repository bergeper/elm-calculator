module Main exposing (..)
import Browser
import Html exposing (Html, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)
import View.Header exposing (viewHeader)
import Html exposing (span)
import Html exposing (main_)
import String exposing (dropRight)


-- MODEL

type alias Model =
    { result : Float
    , operator : String
    , valueOne : String
    , valueTwo : String
    , operation : Operation
    , errorMsg : String
    }


type Operation
    = Add
    | Subtract
    | Multiply
    | Divide
    | None


initModel : Model
initModel =
    { result = 0
    , operator = ""
    , valueOne = ""
    , valueTwo = ""
    , operation = None
    , errorMsg = ""
    }


-- UPDATE


type Msg
    = Number String
    | Operator Operation
    | Equals
    | Backspace
    | Clear


update : Msg -> Model -> Model
update msg model =
    case msg of
        Number value ->
           setValues value model

        Operator op ->
            getOperator op model
        
        Equals ->
            calculateResult model

        Backspace ->
            useBackspace model
        
        Clear ->
            initModel

operatorAndBothValuesSet : Model -> Bool
operatorAndBothValuesSet {valueOne, operation, valueTwo } =
    if valueOne /= "" && operation /= None && valueTwo /= "" then
        True
    else
        False

useBackspace : Model -> Model
useBackspace ({operation, valueOne, valueTwo} as model) = 
    if operation == None then
        {model | valueOne = dropRight 1 valueOne}
    else 
        if operation /= None then
            {model | operation = None, operator = ""}
        else
        {model | valueTwo = dropRight 1 valueTwo}

stringToFloat : String -> Float
stringToFloat value = 
     Maybe.withDefault 0 (String.toFloat value)
        

calculateResult : Model -> Model
calculateResult model =
    let

        valueOne =
            stringToFloat model.valueOne
        
        valueTwo =
            stringToFloat model.valueTwo

        operator =
            model.operation
    in
        case operator of 
            Add ->
                {model 
                    | result = valueOne + valueTwo
                    , valueOne = String.fromFloat (valueOne + valueTwo)
                    , valueTwo = ""
                    , operator = ""
                    , operation = None
                    , errorMsg = ""
                    }

            Subtract ->
                {model 
                    | result = valueOne - valueTwo
                    , valueOne = String.fromFloat (valueOne - valueTwo)
                    , valueTwo = ""
                    , operator = ""
                    , operation = None
                    , errorMsg = ""
                     }

            Multiply ->
                {model 
                    | result = valueOne * valueTwo
                    , valueOne = String.fromFloat (valueOne * valueTwo)
                    , valueTwo = ""
                    , operator = ""
                    , operation = None
                    , errorMsg = ""
                     }

            Divide ->
                if (floor (Maybe.withDefault 0 (String.toFloat model.valueTwo))) == 0 then
                    {model 
                        | errorMsg = "Can't Divide by 0"
                        , result = 0
                        , valueOne = ""
                        , valueTwo = ""
                        , operator = ""
                        , operation = None
                    }

                else 
                    {model 
                        | result = valueOne / valueTwo
                        , valueOne = String.fromFloat (valueOne / valueTwo)
                        , valueTwo = ""
                        , operator = ""
                        , operation = None
                        , errorMsg = ""
                    }

            None ->
                model

getOperator : Operation -> Model -> Model
getOperator op model =
    let 
        result =
            operatorAndBothValuesSet model

        calcModel =
            calculateResult model
        
    in

    if result then
        case op of
            Add ->
               
                {model 
                    | result = calcModel.result
                    , valueOne = String.fromFloat (calcModel.result) 
                    , valueTwo = ""
                    , operator = "+"
                    , operation = op
                }

            Subtract ->
                {model 
                    | result = calcModel.result
                    , valueOne = String.fromFloat (calcModel.result)
                    , valueTwo = ""
                    , operator = "-"
                    , operation = op
                }

            Multiply ->
                {model 
                    | result = calcModel.result
                    , valueOne = String.fromFloat (calcModel.result)
                    , valueTwo = ""
                    , operator = "×"
                    , operation = op
                }


            Divide ->
                if (floor (Maybe.withDefault 0 (String.toFloat model.valueTwo))) == 0 then
                    {model 
                        | errorMsg = "Can't Divide by 0"
                        , result = 0
                        , valueOne = ""
                        , valueTwo = ""
                        , operator = ""
                        , operation = None
                    }

                else
                    {model 
                        | result = calcModel.result
                        , valueOne = String.fromFloat (calcModel.result)
                        , valueTwo = ""
                        , operator = "÷"
                        , operation = op                        
                    }

            None ->
                {model | operation = op}

        
    else

            case op of 
                Add ->
                    {model | operation = op, operator = "+" }

                Subtract ->
                    {model | operation = op, operator = "-" }

                Multiply ->
                    {model | operation = op, operator = "×" }

                Divide ->
                    {model | operation = op, operator = "÷" }

                None ->
                    {model | operation = op}



setValues : String -> Model -> Model
setValues value model = 
    case model.operation of
        None ->
            {model | valueOne = model.valueOne ++ value, errorMsg = ""}
        
        _ ->
            {model | valueTwo = model.valueTwo ++ value}

-- VIEW


view : Model -> Html Msg
view model =
    let
        operatorView func op =
             div [ class "calculator__pad--operators", onClick (Operator func)] [text op]

        numView num =
            div [ class "calculator__pad--number", onClick (Number num)] [text num]

        choiceView func str =
            div [ class "calculator__pad--choice", onClick func] [text str] 

        row html  = 
            div [ class "calculator__padRow"]  html 

        zeroView = 
            div [ class "calculator__pad--zero", onClick (Number "0")] [text "0"]

        equalsView =
            div [ class "calculator__pad--equals", onClick Equals] [text "="]
    in
    
    div []
        [    viewHeader
            , main_ [ class "container" ] [
                div [ class "program"] [
                    div [ class "calculator__input"] [ 
                         span [ class "calculator__errorMsg" ][ text model.errorMsg]
                        , span [ class "calculator__valueOne" ][ text model.valueOne]
                        , div [ ][ 
                            span [ class "calculator__valueTwo" ] [ text model.operator]
                            , span [ class "calculator__valueTwo" ] [ text model.valueTwo]
                        ]
                    ]
                    , div [ class "calculator" ] [
                         row 
                            [ 
                                choiceView Clear "C"
                                , choiceView Backspace "<"
                                , operatorView Divide "÷"
                            ]
                        , row 
                            [ 
                                numView "7"
                                , numView "8"
                                , numView "9"
                                , operatorView Multiply "×"

                            ]
                        , row 
                            [ 
                                numView "4" 
                                , numView "5"
                                , numView "6"
                                , operatorView Subtract "-"

                            ]
                        , row 
                            [ 
                                numView "1"
                                , numView "2"
                                , numView "3"
                                , operatorView Add "+"
                            ]
                        , row 
                            [ 
                                zeroView
                                , equalsView
                            ]
                    ]
                ]
            ]
        ]


-- MAIN
main : Program () Model Msg
main =
  Browser.sandbox { init = initModel, update = update, view = view }
