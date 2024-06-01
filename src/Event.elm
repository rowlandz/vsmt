module Event exposing (..)

type Event
  = UserChangedText String
  | UserChangedSortName Int String
  | UserChangedVarName Int String
  | UserChangedVarType Int String
  | UserDeletedVar Int
  | UserAddedVar
  | UserClickedTactic String