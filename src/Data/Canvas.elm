module Data.Canvas exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Data.Typechecked exposing (Sort, FuncType, ExprT)

type Canvas
  = MkCEntry CEntry
  | MkCTopLevelExpr CTopLevelExpr
  | MkCDPLL CDPLL

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
  , boundVars : List ( String, ExprT )
  }

type alias DPLLBranch = List DPLLClause

type alias DPLLClause = List DPLLAtom

type alias DPLLAtom =
  { get : String
  , negated : Bool
  }



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