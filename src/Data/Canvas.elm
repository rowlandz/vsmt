module Data.Canvas exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Common exposing (listGet, listSet)
import Data.Typechecked exposing (Sort, FuncType, ExprT)

type Canvas
  = MkCEntry CEntry
  | MkCTopLevelExpr CTopLevelExpr
  | MkCDPLL CDPLL
  | MkCUnsat

type alias CEntry =
  { uninterpSorts : Array String
  , variables : Array ( String, String )
  , expr : String
  }

type alias CTopLevelExpr =
  { varContext : VarContext
  , expr : ExprT
  }



-- DPLL


type alias CDPLL =
  { varContext : VarContext
  , branches : List DPLLBranch
  , activeBranch : Int
  , boundVars : List ( String, ExprT )
  }

type alias DPLLBranch =
  { clauses : List DPLLClause
  , partialSol : List DPLLAtom
  }

type alias DPLLClause = List DPLLAtom

type alias DPLLAtom =
  { get : String
  , negated : Bool
  }

activeBranch : CDPLL -> Maybe DPLLBranch
activeBranch dpll =
  listGet dpll.activeBranch dpll.branches

setActiveBranch : CDPLL -> DPLLBranch -> CDPLL
setActiveBranch dpll branch =
  { dpll | branches = listSet dpll.activeBranch branch dpll.branches }


-- Shared


type alias VarContext =
  { freeVars : Dict String Sort
  , freeFuns : Dict String FuncType
  }

getCanvasType : Canvas -> String
getCanvasType canvas =
  case canvas of
    MkCEntry _        -> "Entry"
    MkCTopLevelExpr _ -> "TopLevelExpr"
    MkCDPLL _         -> "DPLL"
    MkCUnsat          -> "Unsat"