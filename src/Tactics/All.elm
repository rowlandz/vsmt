module Tactics.All exposing (..)

import Tactic exposing (Tactic)
import Tactics.TacEntry as TacEntry
import Tactics.TacTopLevelExpr as TacTopLevelExpr
import Tactics.TacCNF as TacCNF
import Tactics.TacDPLL as TacDPLL

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
  , TacTopLevelExpr.tacCheckCNF
  , TacCNF.tacExtractProps
  , TacCNF.tacPurify
  , TacDPLL.tacSplitOn
  , TacDPLL.tacPropagateUnit
  , TacDPLL.tacRemoveDuplicateAtoms
  , TacDPLL.tacFoundEmptyClause
  , TacDPLL.tacNoBranches
  ]