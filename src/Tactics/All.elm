module Tactics.All exposing (..)

import Tactic exposing (Tactic)
import Tactics.TacEntry as TacEntry

all : List Tactic
all =
  [ TacEntry.parseTopLevelExpr
  ]