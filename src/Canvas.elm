module Canvas exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Data.Typechecked exposing (Sort, FuncType, ExprT)

type Canvas
  = MkCEntry CEntry
  | MkCTopLevelExpr CTopLevelExpr

type alias CEntry =
  { uninterpretedSorts : Array String
  , variables : Array ( String, String )
  , expr : String
  }

type alias CTopLevelExpr =
  { varContext : VarContext
  , expr : ExprT
  }

type alias VarContext =
  { freeVars : Dict String Sort
  , freeFuns : Dict String FuncType
  }