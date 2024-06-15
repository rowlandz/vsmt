module Tactics.TacDPLL exposing (..)

import Common exposing (listDelete, listFindFirstWhere, listOneMustSucceed, listRemoveDuplicates, listReplace, listSplitOnFirst)
import Data.Canvas exposing (Canvas(..), DPLLBranch, DPLLClause, DPLLAtom, activeBranch, setActiveBranch)
import Tactic exposing (Tactic)



-- Tactics


tacSplitOn : Tactic
tacSplitOn =
  { name = "split on"
  , fromCanvas = "DPLL"
  , params = [ "x" ]
  , run = \args c -> case c of
      MkCDPLL dpll -> case activeBranch dpll of
        Just branch -> case args of
          [ x ] ->
            let ( b1, b2 ) = splitOn x branch in
            Ok (MkCDPLL { dpll | branches = listReplace dpll.activeBranch [ b1, b2 ] dpll.branches })
          _ -> Err "Unexpected arguments"
        Nothing -> Err "Must have at least one branch"
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
          Just branch ->
            propagateUnit x branch
            |> Result.map (setActiveBranch dpll >> MkCDPLL)
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
        Just branch -> case listOneMustSucceed removeDuplicateAtoms branch.clauses of
          Just newClauses ->
            Ok (MkCDPLL (setActiveBranch dpll { branch | clauses = newClauses }))
          Nothing -> Err "Nothing to simplify"
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
        Just branch ->
          if List.any List.isEmpty branch.clauses then
            Ok (MkCDPLL { dpll | branches = listDelete dpll.activeBranch dpll.branches })
          else Err "No empty clause found"
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


-- Helper functions for tactics


{-| Tries to perform unit propagation with `var` or `(not var)`. -}
propagateUnit : String -> DPLLBranch -> Result String DPLLBranch
propagateUnit var branch =
  case listFindFirstWhere (isUnitClauseOf var) branch.clauses of
    Nothing -> Err ("Could not find unit clause of variable " ++ var)
    Just ( left, unit, right ) ->
      { partialSol = branch.partialSol ++ [ unit ]
      , clauses = List.filterMap (simplifyClauseAssuming unit) (left ++ right)
      } |> Ok

{-| Checks if the `clause` is one of the unit clauses `var` or `(not var)`.
Returns the atom on success, `Nothing` on failure. -}
isUnitClauseOf : String -> DPLLClause -> Maybe DPLLAtom
isUnitClauseOf var clause =
  case clause of
    [ atom ] -> if atom.prop == var then Just atom else Nothing
    _ -> Nothing

{-| Applies the splitting rule. -}
splitOn : String -> DPLLBranch -> ( DPLLBranch, DPLLBranch )
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
