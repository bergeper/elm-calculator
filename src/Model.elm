module Model exposing (Model, Operation(..), initModel)


type Operation
    = Add
    | Subtract
    | Multiply
    | Divide
    | None


type alias Model =
    { result : Float
    , operator : String
    , valueOne : String
    , valueTwo : String
    , operation : Operation
    , errorMsg : String
    }


initModel : Model
initModel =
    { result = 0
    , operator = ""
    , valueOne = ""
    , valueTwo = ""
    , operation = None
    , errorMsg = ""
    }
