module ExprT exposing (..)

type ExprT = ExprT String Sort (List ExprT)

getSort : ExprT -> Sort
getSort (ExprT _ sort _) = sort

type Sort
  = BoolSort
  | RealSort
  | UninterpSort String

type alias FuncType =
  { paramSorts : List Sort
  , retSort : Sort
  }