module Main exposing (..)
import Browser
import Model exposing (Model, Operation(..), initModel)
import Update exposing (update, Msg(..))
import View exposing (view)



-- MAIN
main : Program () Model Msg
main =
  Browser.sandbox { init = initModel, update = update, view = view }
