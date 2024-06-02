module Event exposing (..)

type Event
  = UserAddedSort
  | UserChangedSortName Int String
  | UserDeletedSort Int
  | UserAddedVar
  | UserChangedVarName Int String
  | UserChangedVarType Int String
  | UserDeletedVar Int
  | UserChangedText String
  | UserClickedTactic String