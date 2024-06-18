module Tactics.TacDPLL exposing (..)

import Common exposing (greedy1, listDelete, listFindFirstWhere, listOneMustSucceed, listRemoveDuplicates, listReplace, listSplitOnFirst)
import Data.Canvas exposing (Canvas(..), DPLLClause, DPLLAtom, activeBranch, setActiveBranch)
import Tactic exposing (Tactic)
import Data.Canvas exposing (SATBranch)
import Data.Canvas exposing (Branch(..))



-- Tactics


tacSplitOn : Tactic
tacSplitOn =
  { name = "split on"
  , fromCanvas = "DPLL"
  , params = [ "x" ]
  , run = \args c -> case c of
      MkCDPLL dpll -> case activeBranch dpll of
        Just (MkSATBranch branch) -> case args of
          [ x ] ->
            let ( b1, b2 ) = splitOn x branch in
            Ok (MkCDPLL { dpll | branches = listReplace dpll.activeBranch [ MkSATBranch b1, MkSATBranch b2 ] dpll.branches })
          _ -> Err "Unexpected arguments"
        Just _ -> Err "Expected SAT branch"
        Nothing -> Err "Must have at least one branch"
      _ -> Err "Wrong canvas type"
  }

tacPropagateUnits : Tactic
tacPropagateUnits =
  { name = "propagate units"
  , fromCanvas = "DPLL"
  , params = []
  , run = \_ c -> case c of
      MkCDPLL dpll -> case activeBranch dpll of
        Just (MkSATBranch branch) -> case greedy1 propagateFirstUnit branch of
          Just newBranch -> Ok (MkCDPLL (setActiveBranch dpll (MkSATBranch newBranch)))
          Nothing -> Err "No unit clauses to propagate"
        Just _ -> Err "Expected SAT branch"
        Nothing -> Err "Must have at least one branch to propagate units"
      _ -> Err "Wrong canvas type"
  }

tacPropagateUnit : Tactic
tacPropagateUnit =
  { name = "propagate unit"
  , fromCanvas = "DPLL"
  , params = [ "x" ]
  , run = \args c -> case c of
      MkCDPLL dpll -> case args of
        [ x ] -> case activeBranch dpll of
          Just (MkSATBranch branch) ->
            propagateUnit x branch
            |> Result.map (MkSATBranch >> setActiveBranch dpll >> MkCDPLL)
          Just _ -> Err "Expected SAT branch"
          Nothing -> Err "Must have at least one branch to propagate unit"
        _ -> Err "Invalid arguments"
      _ -> Err "Wrong canvas type"
  }

tacRemoveDuplicateAtoms : Tactic
tacRemoveDuplicateAtoms =
  { name = "remove duplicate atoms"
  , fromCanvas = "DPLL"
  , params = []
  , run = \_ c -> case c of
      MkCDPLL dpll -> case activeBranch dpll of
        Just (MkSATBranch branch) -> case listOneMustSucceed removeDuplicateAtoms branch.clauses of
          Just newClauses ->
            Ok (MkCDPLL (setActiveBranch dpll (MkSATBranch { branch | clauses = newClauses })))
          Nothing -> Err "Nothing to simplify"
        Just _ -> Err "Expected SAT branch"
        Nothing -> Err "Must have at least one branch"
      _ -> Err "Wrong canvas type"
  }

tacFoundEmptyClause : Tactic
tacFoundEmptyClause =
  { name = "found empty clause"
  , fromCanvas = "DPLL"
  , params = []
  , run = \_ c -> case c of
      MkCDPLL dpll -> case activeBranch dpll of
        Just (MkSATBranch branch) ->
          if List.any List.isEmpty branch.clauses then
            Ok (MkCDPLL { dpll | branches = listDelete dpll.activeBranch dpll.branches })
          else Err "No empty clause found"
        Just _ -> Err "Expected SAT branch"
        Nothing -> Err "Must have at least one branch"
      _ -> Err "Wrong canvas type"
  }

tacNoBranches : Tactic
tacNoBranches =
  { name = "no branches"
  , fromCanvas = "DPLL"
  , params = []
  , run = \_ c -> case c of
      MkCDPLL dpll -> case dpll.branches of
        [] -> Ok MkCUnsat
        _ -> Err "There are branchces"
      _ -> Err "Wrong canvas type"
  }

tacStartTheorySolvers : Tactic
tacStartTheorySolvers =
  { name = "start theory solvers"
  , fromCanvas = "DPLL"
  , params = []
  , run = \_ c -> case c of
      MkCDPLL dpll -> case activeBranch dpll of
        Just (MkSATBranch branch) ->
          if List.length branch.clauses == 0 then
            if List.length dpll.theoryProps > 0 then
              Err "Unimpl"
            else Err "There are no theory props, so no need to start theory solvers"
          else Err "There are still clauses left"
        Just _ -> Err "Expected a SAT branch"
        Nothing -> Err "There is no active branch"
      _ -> Err "Wrong canvas type"
  }


-- Helper functions for tactics


{-| Tries to perform unit propagation on the first unit clause it can find. -}
propagateFirstUnit : SATBranch -> Maybe SATBranch
propagateFirstUnit branch =
  listFindFirstWhere isUnitClause branch.clauses |> Maybe.map
    (\( left, unit, right ) ->
      { partialSol = branch.partialSol ++ [ unit ]
      , clauses = List.filterMap (simplifyClauseAssuming unit) (left ++ right)
      }
    )


{-| Tries to perform unit propagation with `var` or `(not var)`. -}
propagateUnit : String -> SATBranch -> Result String SATBranch
propagateUnit var branch =
  case listFindFirstWhere (isUnitClauseOf var) branch.clauses of
    Nothing -> Err ("Could not find unit clause of variable " ++ var)
    Just ( left, unit, right ) ->
      { partialSol = branch.partialSol ++ [ unit ]
      , clauses = List.filterMap (simplifyClauseAssuming unit) (left ++ right)
      } |> Ok

{-| Checks if `clause` is a unit clause. -}
isUnitClause : DPLLClause -> Maybe DPLLAtom
isUnitClause clause =
  case clause of
    [ atom ] -> Just atom
    _ -> Nothing

{-| Checks if the `clause` is one of the unit clauses `var` or `(not var)`.
Returns the atom on success, `Nothing` on failure. -}
isUnitClauseOf : String -> DPLLClause -> Maybe DPLLAtom
isUnitClauseOf var clause =
  case clause of
    [ atom ] -> if atom.prop == var then Just atom else Nothing
    _ -> Nothing

{-| Applies the splitting rule. -}
splitOn : String -> SATBranch -> ( SATBranch, SATBranch )
splitOn var branch =
  let varTrue = { prop = var, negated = False }
      varFalse = { prop = var, negated = True } in
  Tuple.pair
    { partialSol = branch.partialSol ++ [ varTrue ]
    , clauses = List.filterMap (simplifyClauseAssuming varTrue) branch.clauses
    }
    { partialSol = branch.partialSol ++ [ varFalse ]
    , clauses = List.filterMap (simplifyClauseAssuming varFalse) branch.clauses
    }

{-| Simplifies `clause` under the `assump`-tion. Returns `Nothing`
if the clause simplifies to `true`. -}
simplifyClauseAssuming : DPLLAtom -> DPLLClause -> Maybe DPLLClause
simplifyClauseAssuming assump clause =
  case listSplitOnFirst (\atom -> atom == assump) clause of
    Just _ -> Nothing
    Nothing -> Just (List.filter (\atom -> atom.prop /= assump.prop) clause)

{-| Removes duplicate atoms from a clause. Returns `Nothing` if
no simplification can be performed. -}
removeDuplicateAtoms : DPLLClause -> Maybe DPLLClause
removeDuplicateAtoms clause =
  let simpl = listRemoveDuplicates clause in
  if List.length simpl < List.length clause then Just simpl else Nothing
