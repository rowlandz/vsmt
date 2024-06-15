module Data.Canvas exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Common exposing (listGet, listSet)
import Data.Typechecked exposing (Sort, FuncType, ExprT)

type Canvas
  = MkCEntry CEntry
  | MkCTopLevelExpr CTopLevelExpr
  | MkCCNF1 CCNF1
  | MkCCNF2 CCNF2
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



-- CNF


type alias Atom a =
  { prop : a
  , negated : Bool
  }

type alias CCNF1 =
  { varContext : VarContext
  , clauses : List (List (Atom ExprT))
  }

type alias CCNF2 =
  { varContext : VarContext
  , clauses : List (List (Atom String))
  , binds : List ( String, ExprT )
  , firstUnused : Int
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
  , partialSol : List (Atom String)
  }

type alias DPLLClause = List (Atom String)

type alias DPLLAtom = Atom String

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
    MkCCNF1 _         -> "CNF1"
    MkCCNF2 _         -> "CNF2"
    MkCDPLL _         -> "DPLL"
    MkCUnsat          -> "Unsat"