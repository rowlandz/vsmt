module Tactics.TacTopLevelExpr exposing
  ( tacPropagateNOTs
  , tacFlattenAssoc
  , tacDistributeOROverAND
  )

import Common exposing (greedy, greedy1, orElse, listSplitOnFirst)
import Data.Typechecked exposing (ExprT(..), Sort(..), getHead, getSort, getSubs)
import Canvas exposing (Canvas(..))
import Tactic exposing (Tactic)



-- Tactics


tacPropagateNOTs : Tactic
tacPropagateNOTs =
  { name = "propagate not"
  , fromCanvas = "TopLevelExpr"
  , run = \c -> case c of
      MkCTopLevelExpr tle ->
        case topDownGreedy1Rewrite propagateNot tle.expr of
          Just newExpr -> Ok (MkCTopLevelExpr { tle | expr = newExpr })
          Nothing -> Err "Tactic failed to rewrite anything"
      _ -> Err "Wrong canvas type"
  }

tacFlattenAssoc : Tactic
tacFlattenAssoc =
  { name = "flatten ands and ors"
  , fromCanvas = "TopLevelExpr"
  , run = \c -> case c of
      MkCTopLevelExpr tle ->
        case topDownGreedy1Rewrite flattenAssoc tle.expr of
          Just newExpr -> Ok (MkCTopLevelExpr { tle | expr = newExpr })
          Nothing -> Err "Tactic failed to rewrite anything"
      _ -> Err "Wrong canvas type"
  }

tacDistributeOROverAND : Tactic
tacDistributeOROverAND =
  { name = "distribute or over and"
  , fromCanvas = "TopLevelExpr"
  , run = \c -> case c of
      MkCTopLevelExpr tle ->
        case topDownGreedy1Rewrite (distributeXOverY "or" "and") tle.expr of
          Just newExpr -> Ok (MkCTopLevelExpr { tle | expr = newExpr })
          Nothing -> Err "Tactic failed to rewrite anything"
      _ -> Err "Wrong canvas type"
  }



-- Helper functions for the tactics


{-| Apply `greedy rewrite` to every node in the expression tree in a top-down
order. `rewrite` should be sort-preserving.
-}
topDownGreedyRewrite : (ExprT -> Maybe ExprT) -> ExprT -> ExprT
topDownGreedyRewrite rewrite expr =
  let (ExprT nd st subExprs) = greedy rewrite expr in
  ExprT nd st (List.map (topDownGreedyRewrite rewrite) subExprs)

{-| Apply `greedy1 rewrite` to every node in the expression tree in a top-down
order. Returns `Just` a value if `rewrite` succeeds for _any_ node.
-}
topDownGreedy1Rewrite : (ExprT -> Maybe ExprT) -> ExprT -> Maybe ExprT
topDownGreedy1Rewrite rewrite expr =
  case greedy1 rewrite expr of
    Just (ExprT hd st subExprs) -> Just (ExprT hd st (List.map (topDownGreedyRewrite rewrite) subExprs))
    Nothing ->
      let rewrittenSubs = List.map (topDownGreedy1Rewrite rewrite) (getSubs expr) in
      if List.all (\m -> m == Nothing) rewrittenSubs then Nothing
      else
        List.map2 Maybe.withDefault (getSubs expr) rewrittenSubs
        |> ExprT (getHead expr) (getSort expr)
        |> Just
        

propagateNot : ExprT -> Maybe ExprT
propagateNot expr =
  distributeNotOverAnd expr
  |> orElse (\_ -> distributeNotOverOr expr)
  |> orElse (\_ -> removeDoubleNot expr)

distributeNotOverAnd : ExprT -> Maybe ExprT
distributeNotOverAnd expr =
  case expr of
    ExprT "not" _ [ ExprT "and" _ subExprs ] ->
      Just <| ExprT "or" BoolSort (List.map (List.singleton >> ExprT "not" BoolSort) subExprs)
    _ -> Nothing

distributeNotOverOr : ExprT -> Maybe ExprT
distributeNotOverOr expr =
  case expr of
    ExprT "not" _ [ ExprT "or" _ subExprs ] ->
      Just <| ExprT "and" BoolSort (List.map (List.singleton >> ExprT "not" BoolSort) subExprs)
    _ -> Nothing

removeDoubleNot : ExprT -> Maybe ExprT
removeDoubleNot expr =
  case expr of
    ExprT "not" _ [ ExprT "not" _ [ subExpr ] ] -> Just subExpr
    _ -> Nothing

flattenAssoc : ExprT -> Maybe ExprT
flattenAssoc expr =
  flattenX "and" expr
  |> orElse (\_ -> flattenX "or" expr)
  |> orElse (\_ -> flattenX "+" expr)
  |> orElse (\_ -> flattenX "*" expr)


{-| Flattens associative expression forms like `and`, `or`, `+`, `*`.
Only one layer of flattening is performed.

       (x e0 ..(x e1).. e2)
    ~> (x e0 e1 e2)
-}
flattenX : String -> ExprT -> Maybe ExprT
flattenX x (ExprT hd st subExprs) =
  if hd == x && List.any (\e -> getHead e == x) subExprs then
    subExprs
    |> List.concatMap (\e -> if getHead e == x then getSubs e else [ e ])
    |> ExprT x st
    |> Just
  else Nothing

{-| Rewrites expressions that follow a distributive rule such
as `or` over `and`, `*` over `+`. Only the first `y` sub-expression
is distributed over. `x` and `y` must operate over a single sort.

       (x e0 ..(y e1 e2).. e3)
    ~> (y (x e0 ..e1.. e3) (x e0 ..e2.. e3))
-}
distributeXOverY : String -> String -> ExprT -> Maybe ExprT
distributeXOverY x y (ExprT hd st subExprs) =
  if hd == x then
    listSplitOnFirst (\e -> getHead e == y) subExprs |> Maybe.map
      (\( subs1, (ExprT _ _ ySubs), subs2 ) ->
        ySubs
        |> List.map (\ySub -> ExprT x st (subs1 ++ ySub :: subs2))
        |> ExprT y st
      )
  else Nothing
