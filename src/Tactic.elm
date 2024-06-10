module Tactic exposing (..)

import Data.Canvas exposing (Canvas)

type alias Tactic =
  { name : String
  , fromCanvas : String
  , params : List String
  , run : List String -> Canvas -> Result String Canvas
  }