module Data.Typechecked exposing (..)

{-| Function types, sorts, annotated expressions, and
type checking errors.
-}

import Dict exposing (Dict)
import Data.Parsed exposing (Location)

{-| An expression annotated with sorts.
-}
type ExprT = ExprT String Sort (List ExprT)

getSort : ExprT -> Sort
getSort (ExprT _ sort _) = sort

exprTToString : ExprT -> String
exprTToString (ExprT head _  children) =
  case children of
    [] -> head
    _ -> children
      |> List.map exprTToString
      |> (\l -> head :: l)
      |> List.intersperse " "
      |> String.concat
      |> (\s -> "(" ++ s ++ ")")

{-| The "type" of an expression.
-}
type Sort
  = BoolSort
  | RealSort
  | UninterpSort String

{-| The "type" of a function.
-}
type alias FuncType =
  { paramSorts : List Sort
  , retSort : Sort
  }

{-| Free variables and functions.
-}
type alias VarContext =
  { freeVars : Dict String Sort
  , freeFuns : Dict String FuncType
  }

{-| Typechecking errors
-}
type TCErr
  = UndeclaredSymbol Location
  | SortMismatch Location Sort Sort
  | ArityMismatch Location Int Int
  | SortsShouldMatch Sort Location Sort Location
  | UnrecognizedHeadSymbol Location
  | UnknownExpressionForm Location