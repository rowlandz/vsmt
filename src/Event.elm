module Event exposing (..)

import Model exposing (TacticSelector)

type Event
  = UserAddedSort
  | UserChangedSortName Int String
  | UserDeletedSort Int
  | UserAddedVar
  | UserChangedVarName Int String
  | UserChangedVarType Int String
  | UserDeletedVar Int
  | UserChangedText String
  | UserClickedDPLLTab Int
  | UserClickedShowHideTheoryProps Bool
  | UserChangedTacticArg Int Int String
  | UserClickedTactic TacticSelector
  | UserClickedUndo
  | UserSelectedExample String
  | DoNothing