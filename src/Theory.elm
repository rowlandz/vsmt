module Theory exposing (..)

import Common exposing (ST, getThenST, modifyThenST, returnST, traverseThenST)
import Data.Canvas exposing (Canvas)
import Data.Typechecked exposing (ExprT(..), Sort(..), VarContext, getHead, getSort, getSubs)

type alias Theory =
  { name : String
  , belongs : VarContext -> ExprT -> Bool
  , purify : ExprT -> ST SpliceST ExprT
  , init : List ExprT -> Result (List String) Canvas
  }

{-| Context for expression splicing.
  * `varContext` -- (read only) user-entered free variables and functions
  * `firstUnused` -- (read/write) should count up to generate fresh names
  * `spliced` -- (write only) should be set to spliced-out sub-exprs
-}
type alias SpliceST =
  { varContext : VarContext
  , firstUnused : Int
  , spliced : List ( String, ExprT )
  }

{-| Assigns `expr` to a fresh name in `spliced` and returns the fresh name as
an expression. -}
assignNew : ExprT -> ST SpliceST ExprT
assignNew expr =
  getThenST <| \{ firstUnused, spliced } ->
  let freshName = "$" ++ String.fromInt firstUnused
      newEq = ( freshName, expr ) in
  modifyThenST (\s -> { s | firstUnused = firstUnused + 1 }) <|
  modifyThenST (\s -> { s | spliced = spliced ++ [ newEq ] }) <|
  returnST (ExprT freshName (getSort expr) [])

{-| Returns `expr` with its sub-expressions purified with `recurse`. -}
purifySubExprs : (ExprT -> ST s ExprT) -> ExprT -> ST s ExprT
purifySubExprs recurse expr =
  traverseThenST recurse (getSubs expr) <| \pureSubs ->
  returnST (ExprT (getHead expr) (getSort expr) pureSubs)