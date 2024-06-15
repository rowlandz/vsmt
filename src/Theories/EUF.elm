module Theories.EUF exposing (euf)

import Data.Typechecked exposing (ExprT(..), Sort(..))
import Theory exposing (Theory)

euf : Theory
euf =
  { name = "EUF"
  , belongs = \(ExprT head sort _) ->
      case sort of
        UninterpSort _ -> True
        BoolSort -> not (List.member head [ "<", ">", "<=", ">=" ])
        RealSort ->
          if List.member head [ "+", "-", "*", "neg" ] then
            False
          else case String.toInt head of
            Just _ -> False
            Nothing -> True
  }