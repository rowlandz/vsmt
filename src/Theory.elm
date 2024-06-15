module Theory exposing (..)

import Data.Typechecked exposing (ExprT)

type alias Theory =
  { name : String
  , belongs : ExprT -> Bool
  }