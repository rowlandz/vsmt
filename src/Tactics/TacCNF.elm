module Tactics.TacCNF exposing (tacExtractProps, tacPurify)

import Common exposing (ST, listSplitOnFirst, getThenST, modifyThenST, returnST, traverseST, traverseThenST)
import Data.Canvas exposing (Canvas(..), Atom)
import Data.Typechecked exposing (ExprT(..), Sort(..), exprTEq, getHead, getSort, getSubs)
import Tactic exposing (Tactic)
import Theories.All
import Common exposing (listFindFirstWhere)

tacExtractProps : Tactic
tacExtractProps =
  { name = "extract propositions"
  , fromCanvas = "CNF1"
  , params = []
  , run = \_ c -> case c of
      MkCCNF1 cnf ->
        let ( extracted, st ) = extractClauses cnf.clauses { binds = [], firstUnused = 1 } in
        Ok (MkCCNF2 { varContext = cnf.varContext, clauses = extracted, binds = st.binds, firstUnused = st.firstUnused })
      _ -> Err "Wrong canvas type"
  }

tacPurify : Tactic
tacPurify = 
  { name = "purify"
  , fromCanvas = "CNF2"
  , params = []
  , run = \_ c -> case c of
      MkCCNF2 cnf ->
        let st1 = { firstUnused = cnf.firstUnused, equalities = [] }
            ( purified, st2 ) = purifyExprs (List.map Tuple.second cnf.binds) st1
            oldBinds = List.map2 Tuple.pair (List.map Tuple.first cnf.binds) purified
            ( newBinds, _ ) = equalitiesToBinds st2.equalities st2.firstUnused in
        { varContext = cnf.varContext
        , branches = [ { clauses = cnf.clauses, partialSol = [] } ]
        , activeBranch = 0
        , boundVars = oldBinds ++ newBinds
        } |> MkCDPLL |> Ok
      _ -> Err "Wrong canvas type"
  }



-- Extraction


type alias ExtractST =
  { binds : List ( String, ExprT )
  , firstUnused : Int
  }

extractClauses : List (List (Atom ExprT)) -> ST ExtractST (List (List (Atom String)))
extractClauses =
  traverseST <|
  traverseST <|
  \atom -> case atom.prop of
    ExprT p BoolSort [] -> returnST { prop = p, negated = atom.negated }
    _ ->
      getThenST <| \{ binds, firstUnused } ->
      case listSplitOnFirst (Tuple.second >> exprTEq atom.prop) binds of
        Just ( _, ( atomName, _ ), _ ) ->
          returnST { prop = atomName, negated = atom.negated }
        Nothing ->
          let atomName = "$" ++ String.fromInt firstUnused in
          modifyThenST (\s -> { s | firstUnused = s.firstUnused + 1 }) <|
          modifyThenST (\s -> { s | binds = ( atomName, atom.prop ) :: s.binds }) <|
          returnST { prop = atomName, negated = atom.negated }



-- Purification

type alias PurifyST =
  { firstUnused : Int
  , equalities : List ExprT
  }

theoryOf : ExprT -> String
theoryOf e =
  case  
    Theories.All.all |> listFindFirstWhere
      (\theory -> if theory.belongs e then Just theory.name else Nothing)
  of
    Just ( _, theory, _ ) -> theory
    Nothing -> "Unknown"

purifyExprs : List ExprT -> ST PurifyST (List ExprT)
purifyExprs =
  traverseST <|
  \expr ->
    traverseThenST (purify (theoryOf expr)) (getSubs expr) <| \subs ->
    returnST (ExprT (getHead expr) (getSort expr) subs)

purify : String -> ExprT -> ST PurifyST ExprT
purify contextTheory expr =
  if List.isEmpty (getSubs expr) then
    returnST expr
  else
    let exprTheory = theoryOf expr in
    traverseThenST (purify exprTheory) (getSubs expr) <| \subs ->
    let newExpr = ExprT (getHead expr) (getSort expr) subs in
    if exprTheory == contextTheory then
      returnST newExpr
    else
      getThenST <| \{ firstUnused, equalities } ->
      let newVar = "$" ++ String.fromInt firstUnused
          eqExpr = ExprT "=" BoolSort [ ExprT newVar (getSort expr) [], newExpr ] in
      modifyThenST (\s -> { s | firstUnused = firstUnused + 1 }) <|
      modifyThenST (\s -> { s | equalities = eqExpr :: equalities }) <|
      returnST (ExprT newVar (getSort expr) [])

equalitiesToBinds : List ExprT -> ST Int (List ( String, ExprT ))
equalitiesToBinds =
  traverseST <|
  \eq ->
    getThenST <| \i ->
    modifyThenST (\_ -> i + 1) <|
    returnST ( "$" ++ String.fromInt i, eq )
