module Tactics.TacTopLevelExpr exposing
  ( tacPropagateNOTs
  , tacFlattenAssoc
  , tacDistributeOROverAND
  , tacRewriteImplies
  , tacLiftITE
  , tacRewriteBoolITE
  , tacEqToBiImpl
  , tacStartDPLL
  )

import Common exposing (allMustSucceed, greedy, greedy1, listSplitOnFirst, listTraverseST, orElse, yielding)
import Data.Typechecked exposing (ExprT(..), Sort(..), exprTEq, getHead, getSort, getSubs)
import Data.Canvas exposing (Canvas(..), DPLLBranch, DPLLAtom, DPLLClause)
import Tactic exposing (Tactic)



-- Tactics


tacPropagateNOTs : Tactic
tacPropagateNOTs =
  { name = "propagate not"
  , fromCanvas = "TopLevelExpr"
  , params = []
  , run = \_ c -> case c of
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
  , params = []
  , run = \_ c -> case c of
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
  , params = []
  , run = \_ c -> case c of
      MkCTopLevelExpr tle ->
        case topDownGreedy1Rewrite (distributeXOverY "or" "and") tle.expr of
          Just newExpr -> Ok (MkCTopLevelExpr { tle | expr = newExpr })
          Nothing -> Err "Tactic failed to rewrite anything"
      _ -> Err "Wrong canvas type"
  }

tacRewriteImplies : Tactic
tacRewriteImplies =
  { name = "rewrite implies"
  , fromCanvas = "TopLevelExpr"
  , params = []
  , run = \_ c -> case c of
    MkCTopLevelExpr tle ->
      case topDownGreedy1Rewrite rewriteImplies tle.expr of
        Just newExpr -> Ok (MkCTopLevelExpr { tle | expr = newExpr })
        Nothing -> Err "Tactic failed to rewrite anything"
    _ -> Err "Wrong canvas type"
  }

tacLiftITE : Tactic
tacLiftITE =
  { name = "lift if-else"
  , fromCanvas = "TopLevelExpr"
  , params = []
  , run = \_ c -> case c of
    MkCTopLevelExpr tle ->
      case topDownGreedy1Rewrite liftITE tle.expr of  -- TODO: should be bottom-up instead
        Just newExpr -> Ok (MkCTopLevelExpr { tle | expr = newExpr })
        Nothing -> Err "Tactic failed to rewrite anything"
    _ -> Err "Wrong canvas type"
  }

tacRewriteBoolITE : Tactic
tacRewriteBoolITE =
  { name = "rewrite bool if-else"
  , fromCanvas = "TopLevelExpr"
  , params = []
  , run = \_ c -> case c of
    MkCTopLevelExpr tle ->
      case topDownGreedy1Rewrite rewriteBoolITE tle.expr of
        Just newExpr -> Ok (MkCTopLevelExpr { tle | expr = newExpr })
        Nothing -> Err "Tactic failed to rewrite anything"
    _ -> Err "Wrong canvas type"
  }

tacEqToBiImpl : Tactic
tacEqToBiImpl =
  { name = "equal to bi-implication"
  , fromCanvas = "TopLevelExpr"
  , params = []
  , run = \_ c -> case c of
    MkCTopLevelExpr tle ->
      case topDownGreedy1Rewrite eqToBiImpl tle.expr of
        Just newExpr -> Ok (MkCTopLevelExpr { tle | expr = newExpr })
        Nothing -> Err "Tactic failed to rewrite anything"
    _ -> Err "Wrong canvas type"
  }

tacStartDPLL : Tactic
tacStartDPLL =
  { name = "start DPLL"
  , fromCanvas = "TopLevelExpr"
  , params = []
  , run = \_ c -> case c of
    MkCTopLevelExpr tle ->
      case validateCNF tle.expr of
        Ok branch ->
          let st0 = { binds = [], firstUnused = 0 }
              ( st1, dpllBranch ) = extractBranch st0 branch in
              { varContext = tle.varContext
              , branches = [ dpllBranch ]
              , activeBranch = 0
              , boundVars = st1.binds
              } |> MkCDPLL |> Ok
        Err err ->
          ( "Expression not in conjunctive normal form:\n\n"
          ++ String.concat (List.intersperse "\n\n" err)
          ) |> Err
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

rewriteImplies : ExprT -> Maybe ExprT
rewriteImplies (ExprT hd _ subExprs) =
  if hd == "=>" || hd == "implies" then
    case subExprs of
      [ lhs, rhs ] ->
        Just (ExprT "or" BoolSort [ ExprT "not" BoolSort [ lhs ], rhs ])
      _ -> Nothing
  else Nothing

{-| Lifts an `if`/`ite` expression over any other expression. -}
liftITE : ExprT -> Maybe ExprT
liftITE (ExprT hd st subExprs) =
  listSplitOnFirst (\e -> getHead e == "if" || getHead e == "ite") subExprs |> Maybe.andThen
    (\( subs1, iteExpr, subs2 ) ->
      case getSubs iteExpr of
        [ cond, thenExp, elseExp ] ->
          let newThenExp = ExprT hd st (subs1 ++ thenExp :: subs2)
              newElseExp = ExprT hd st (subs1 ++ elseExp :: subs2) in
          Just (ExprT (getHead iteExpr) st [ cond, newThenExp, newElseExp ])
        _ -> Nothing
    )

rewriteBoolITE : ExprT -> Maybe ExprT
rewriteBoolITE (ExprT hd st subExprs) =
  if (hd == "ite" || hd == "if") && st == BoolSort then
    case subExprs of
      [ cond, thenExp, elseExp ] -> Just <|
        ExprT "and" BoolSort
          [ ExprT "or" BoolSort [ ExprT "not" BoolSort [ cond ], thenExp ]
          , ExprT "or" BoolSort [ cond, elseExp ]
          ]
      _ -> Nothing
  else Nothing

eqToBiImpl : ExprT -> Maybe ExprT
eqToBiImpl expr =
  case expr of
    ExprT "=" _ [ p, q ] ->
      if getSort p == BoolSort && getSort q == BoolSort then
        ExprT "and" BoolSort
          [ ExprT "or" BoolSort [ ExprT "not" BoolSort [ p ], q ]
          , ExprT "or" BoolSort [ ExprT "not" BoolSort [ q ], p ]
          ] |> Just
      else Nothing
    _ -> Nothing



-- Validation of CNF

type alias Atom =
  { get : ExprT
  , negated : Bool
  }

validateCNF : ExprT -> Result (List String) (List (List Atom))
validateCNF expr =
  case expr of
    ExprT "and" _ es ->
      allMustSucceed (List.map validateClause es)
    _ ->
      validateClause expr
      |> Result.map List.singleton

validateClause : ExprT -> Result (List String) (List Atom)
validateClause expr =
  case expr of
    ExprT "or" _ subExprs ->
      allMustSucceed (List.map validateAtom subExprs)
    _ ->
      validateAtom expr
      |> Result.map List.singleton

validateAtom : ExprT -> Result (List String) Atom
validateAtom expr =
  case expr of
    ExprT "not" _ [ subExpr ] ->
      validateProp subExpr
      |> yielding { get = subExpr, negated = True }
    _ ->
      validateProp expr
      |> yielding { get = expr, negated = False }

validateProp : ExprT -> Result (List String) ()
validateProp expr =
  if List.member (getHead expr) [ "and", "or", "not", "implies", "=>" ]
  || ((getHead expr == "if" || getHead expr == "ite") && getSort expr == BoolSort) then
    Err [ "Expected an expression not in bool theory" ]
  else
    Ok ()

type alias ExtractST =
  { binds : List ( String, ExprT )
  , firstUnused : Int
  }

extractBranch : ExtractST -> List (List Atom) -> ( ExtractST, DPLLBranch )
extractBranch st llatoms =
  extractClauses st llatoms
  |> Tuple.mapSecond (\clauses -> { partialSol = [], clauses = clauses })

extractClauses : ExtractST -> List (List Atom) -> ( ExtractST, List DPLLClause )
extractClauses = listTraverseST extractClause

extractClause : ExtractST -> List Atom -> ( ExtractST, DPLLClause )
extractClause = listTraverseST extractAtom

extractAtom : ExtractST -> Atom -> ( ExtractST, DPLLAtom )
extractAtom st a =
  case a.get of
    ExprT boolVar BoolSort [] ->
      ( st, { get = boolVar, negated = a.negated } )
    _ -> case listSplitOnFirst (Tuple.second >> exprTEq a.get) st.binds of
      Nothing ->
        let var = "$" ++ (String.fromInt st.firstUnused)
        in  Tuple.pair
              { binds = ( var, a.get ) :: st.binds, firstUnused = st.firstUnused + 1 }
              { get = var, negated = a.negated }
      Just ( b1, b, b2 ) ->
        ( { st | binds = b :: b1 ++ b2 }, { get = Tuple.first b, negated = a.negated } )