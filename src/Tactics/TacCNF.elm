module Tactics.TacCNF exposing (tacExtractProps, tacPurify)

import Common exposing (ST, listSplitOnFirst, getThenST, modifyThenST, returnST, traverseST, traverseThenST)
import Data.Canvas exposing (Canvas(..), Atom, TheoryProp, VarContext)
import Data.Typechecked exposing (ExprT(..), Sort(..), exprTEq, getSort)
import Tactic exposing (Tactic)
import Theories.All

tacExtractProps : Tactic
tacExtractProps =
  { name = "extract propositions"
  , fromCanvas = "CNF1"
  , params = []
  , run = \_ c -> case c of
      MkCCNF1 cnf ->
        let ( extracted, st ) = extractClauses cnf.clauses { binds = [], firstUnused = 1 } in
        { varContext = cnf.varContext
        , clauses = extracted
        , binds = List.sortBy Tuple.first st.binds
        , firstUnused = st.firstUnused
        } |> MkCCNF2 |> Ok
      _ -> Err "Wrong canvas type"
  }

tacPurify : Tactic
tacPurify =
  { name = "purify"
  , fromCanvas = "CNF2"
  , params = []
  , run = \_ c -> case c of
      MkCCNF2 cnf ->
        let st1 = { firstUnused = cnf.firstUnused, theoryEqs = [], varContext = cnf.varContext }
            ( theoryProps, st2 ) = traverseST purify cnf.binds st1
            ( moreTheoryProps, _ ) = theoryEqsToTheoryProps st2.theoryEqs st2.firstUnused
            moreClauses = List.map (\p -> [ { prop = p.name, negated = False } ]) moreTheoryProps
        in
        { varContext = cnf.varContext
        , branches = [ { clauses = cnf.clauses ++ moreClauses, partialSol = [] } ]
        , activeBranch = 0
        , theoryProps = theoryProps ++ moreTheoryProps
        , showTheoryProps = True
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


{-| Context for expression purification.
  * `firstUnused` -- counts up to generate fresh names
  * `equalities` -- (write only) accumulates equality lemmas
  * `varContext` -- (read only) user-entered free variables and functions
-}
type alias PurifyST =
  { varContext : VarContext
  , firstUnused : Int
  , theoryEqs : List TheoryEq
  }

type alias TheoryEq =
  { name : String
  , theory : String
  , expr : ExprT
  }

purify : ( String, ExprT ) -> ST PurifyST TheoryProp
purify ( name, expr ) =
  getThenST <| \st ->
  case
    Theories.All.all |> listSplitOnFirst
      (\theory -> theory.belongs st.varContext expr)
  of
    Just ( _, theory, _ ) ->
      let ( purifiedExpr, spliceST ) = theory.purify expr { varContext = st.varContext, firstUnused = st.firstUnused, spliced = [] }
          theoryProp = { name = name, expr = purifiedExpr, theory = theory.name }
      in
      modifyThenST (\s -> { s | firstUnused = spliceST.firstUnused }) <|
      traverseThenST purify spliceST.spliced <| \theoryEqs ->
      modifyThenST (\s -> { s | theoryEqs = theoryEqs ++ s.theoryEqs }) <|
      returnST theoryProp
    Nothing ->
      returnST { name = name, expr = expr, theory = "???" }

theoryEqsToTheoryProps : List TheoryEq -> ST Int (List TheoryProp)
theoryEqsToTheoryProps =
  traverseST <|
    \eq ->
      getThenST <| \i ->
      modifyThenST (\_ -> i + 1) <|
      returnST
        { name = "$" ++ String.fromInt i
        , theory = eq.theory
        , expr = ExprT "=" BoolSort [ ExprT eq.name (getSort eq.expr) [], eq.expr ]
        }
