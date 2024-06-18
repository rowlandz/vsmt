module Data.Canvas exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Common exposing (listGet, listSet)
import Data.Fract exposing (Fract)
import Data.Typechecked exposing (Sort, FuncType, ExprT)

type Canvas
  = MkCEntry CEntry
  | MkCTopLevelExpr CTopLevelExpr
  | MkCCNF1 CCNF1
  | MkCCNF2 CCNF2
  | MkCDPLL CDPLL
  | MkCLRA CLRA
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
  , branches : List Branch
  , activeBranch : Int
  , theoryProps : List TheoryProp
  , showTheoryProps : Bool
  }

type Branch
  = MkSATBranch SATBranch
  | MkTheoryBranch TheoryBranch

type alias SATBranch =
  { clauses : List DPLLClause
  , partialSol : List (Atom String)
  }

type alias DPLLClause = List (Atom String)

type alias DPLLAtom = Atom String

type alias TheoryProp =
  { name : String
  , theory : String
  , expr : ExprT
  }

type alias TheoryBranch =
  { partialSol : List (Atom String)
  , theoryCanvases : List Canvas
  }

activeBranch : CDPLL -> Maybe Branch
activeBranch dpll =
  listGet dpll.activeBranch dpll.branches

setActiveBranch : CDPLL -> Branch -> CDPLL
setActiveBranch dpll branch =
  { dpll | branches = listSet dpll.activeBranch branch dpll.branches }

partialSolution : Branch -> List (Atom String)
partialSolution branch =
  case branch of
    MkSATBranch b -> b.partialSol
    MkTheoryBranch b -> b.partialSol



-- LRA


type alias CLRA =
  { colLabels : List String
  , tableau : List TableauRow
  }

-- TODO: change number to Fraction
type alias TableauRow = List Fract



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
    MkCLRA _          -> "LRA"
    MkCUnsat          -> "Unsat"