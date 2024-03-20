module Main exposing (..)

import Browser
import Model exposing (Model, Operation(..), initModel)
import Update exposing (Msg(..), update)
import View exposing (view)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = initModel, update = update, view = view }
