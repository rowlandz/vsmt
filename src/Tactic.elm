module Tactic exposing (..)

import Data.Canvas exposing (Canvas)

type alias Tactic =
  { name: String
  , fromCanvas : String
  , run : Canvas -> Result String Canvas
  }