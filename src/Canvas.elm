module Canvas exposing (..)

import Array exposing (Array)

type Canvas
  = MkCEntry CEntry
  | MkCTopLevelExpr

type alias CEntry =
  { uninterpretedSorts : Array String
  , variables : Array ( String, String )
  , expr : String
  }