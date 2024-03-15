module Update exposing (..)
import Model exposing (Model, initModel, Operation(..))
import String exposing (dropRight)


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

