module Main exposing (main)

import Browser
import View exposing (view)
import Model exposing (Model, initialModel)
import Event exposing (Event)
import Update exposing (update)


main : Program () Model Event
main =
  Browser.sandbox
    { init = initialModel
    , update = update
    , view = view
    }


