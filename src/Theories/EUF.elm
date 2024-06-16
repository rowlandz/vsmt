module Theories.EUF exposing (euf)

import Dict
import Data.Typechecked exposing (ExprT(..), Sort(..), getHead)
import Theory exposing (Theory)
import Common exposing (inspectThenST, returnST)
import Theory exposing (assignNew, purifySubExprs)

euf : Theory
euf =
  { name = "EUF"

  , belongs = \varContext expr ->
      if getHead expr == "=" then
        True
      else case Dict.get (getHead expr) varContext.freeVars of
        Just _ -> True
        Nothing -> case Dict.get (getHead expr) varContext.freeFuns of
          Just _ -> True
          Nothing -> False

  , purify = \expr ->
      inspectThenST .varContext <| \varContext ->
      if getHead expr == "=" then
        purifySubExprs euf.purify expr
      else case Dict.get (getHead expr) varContext.freeVars of
        Just _ -> returnST expr
        Nothing -> case Dict.get (getHead expr) varContext.freeFuns of
          Just _ ->
            purifySubExprs euf.purify expr
          Nothing -> assignNew expr
  }