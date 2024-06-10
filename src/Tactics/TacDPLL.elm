module Tactics.TacDPLL exposing (..)

import Tactic exposing (Tactic)
import Data.Canvas exposing (Canvas(..), DPLLBranch, DPLLClause, DPLLAtom)
import Common exposing (listFindFirstWhere, listOneMustSucceed, listRemoveDuplicates, listSplitOnFirst)



-- Tactics


tacPropagateUnit : Tactic
tacPropagateUnit =
  { name = "propagate unit"
  , fromCanvas = "DPLL"
  , params = [ "x" ]
  , run = \args c -> case c of
      MkCDPLL dpll -> case args of
        [ x ] -> case dpll.branches of
          [] -> Err "Must have at least one branch to propagate unit"
          (firstBranch::rest) ->
            propagateUnit x firstBranch
            |> Result.map (\newFirstBranch -> MkCDPLL { dpll | branches = newFirstBranch::rest })
        _ -> Err "Invalid arguments"
      _ -> Err "Wrong canvas type"
  }

tacRemoveDuplicateAtoms : Tactic
tacRemoveDuplicateAtoms =
  { name = "remove duplicate atoms"
  , fromCanvas = "DPLL"
  , params = []
  , run = \_ c -> case c of
      MkCDPLL dpll -> case dpll.branches of
        branch :: rest -> case listOneMustSucceed removeDuplicateAtoms branch of
          Just newBranch -> Ok (MkCDPLL { dpll | branches = newBranch :: rest })
          Nothing -> Err "Nothing to simplify"
        _ -> Err "Must have at least one branch"
      _ -> Err "Wrong canvas type"
  }



-- Helper functions for tactics


propagateUnit : String -> DPLLBranch -> Result String DPLLBranch
propagateUnit unitVar branch =
  case findUnit unitVar branch of
    Nothing -> Err ("Could not find unit clause of variable " ++ unitVar)
    Just ( left, unit, right ) ->
      (left ++ right)
      |> List.filterMap (simplifyClauseAssuming unit)
      |> Ok

{-| Finds a unit clause (either `unit` or its negation) if one exists. -}
findUnit : String -> DPLLBranch -> Maybe ( List DPLLClause, DPLLAtom, List DPLLClause )
findUnit unit branch =
  branch |> listFindFirstWhere
    (\clause -> case clause of
      [ atom ] -> if atom.get == unit then Just atom else Nothing
      _ -> Nothing
    )

{-| Simplifies `clause` under the `assump`-tion. Returns `Nothing`
if the clause simplifies to `true`. -}
simplifyClauseAssuming : DPLLAtom -> DPLLClause -> Maybe DPLLClause
simplifyClauseAssuming assump clause =
  case listSplitOnFirst (\atom -> atom == assump) clause of
    Just _ -> Nothing
    Nothing -> Just (List.filter (\atom -> atom.get /= assump.get) clause)

{-| Removes duplicate atoms from a clause. Returns `Nothing` if
no simplification can be performed. -}
removeDuplicateAtoms : DPLLClause -> Maybe DPLLClause
removeDuplicateAtoms clause =
  let simpl = listRemoveDuplicates clause in
  if List.length simpl < List.length clause then Just simpl else Nothing