module Tactics.All exposing (..)

import Tactic exposing (Tactic)
import Tactics.TacEntry as TacEntry
import Tactics.TacTopLevelExpr as TacTopLevelExpr

all : List Tactic
all =
  [ TacEntry.parseTopLevelExpr
  , TacTopLevelExpr.tacPropagateNOTs
  , TacTopLevelExpr.tacFlattenAssoc
  , TacTopLevelExpr.tacDistributeOROverAND
  , TacTopLevelExpr.tacRewriteImplies
  , TacTopLevelExpr.tacLiftITE
  , TacTopLevelExpr.tacRewriteBoolITE
  , TacTopLevelExpr.tacEqToBiImpl
  , TacTopLevelExpr.tacStartDPLL
  ]