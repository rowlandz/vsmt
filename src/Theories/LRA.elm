module Theories.LRA exposing (lra)

import Data.Canvas exposing (Canvas(..))
import Data.Typechecked exposing (ExprT(..), Sort(..), getHead, getSort, getSubs)
import Theory exposing (Theory, assignNew, purifySubExprs)
import Common exposing (returnST)

lra : Theory
lra =
  { name = "LRA"

  , belongs = \_ expr ->
      if List.member (getHead expr) [ "+", "-", "*", "neg", "<", ">", "<=", ">=" ]
          || isEqualityOfReals expr
      then
        True
      else if isRealLiteral expr then
        True
      else if getSort expr == RealSort && getSubs expr == [] then
        True
      else
        False

  , purify = \expr ->
      if List.member (getHead expr) [ "+", "-", "*", "neg", "<", ">", "<=", ">=" ]
          || isEqualityOfReals expr
      then
        purifySubExprs lra.purify expr
      else if isRealLiteral expr then
        returnST expr
      else if getSort expr == RealSort && getSubs expr == [] then
        returnST expr
      else
        assignNew expr

  , init = \exprs -> Ok (MkCLRA exprs)
  }

isRealLiteral : ExprT -> Bool
isRealLiteral expr =
  case String.toInt (getHead expr) of
    Just _ -> True
    Nothing -> False

isEqualityOfReals : ExprT -> Bool
isEqualityOfReals expr =
  case expr of
    ExprT "=" _ [ ExprT _ RealSort _, ExprT _ RealSort _ ] -> True
    _ -> False