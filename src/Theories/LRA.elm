module Theories.LRA exposing (lra)

import Data.Typechecked exposing (ExprT(..), Sort(..))
import Theory exposing (Theory)

lra : Theory
lra =
  { name = "LRA"
  , belongs = \(ExprT head _ _) ->
      if List.member head [ "+", "-", "*", "neg", "<", ">", "<=", ">=" ] then
        True
      else case String.toInt head of
        Just _ -> True
        Nothing -> False
  }