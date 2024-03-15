module Model exposing (..)

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

