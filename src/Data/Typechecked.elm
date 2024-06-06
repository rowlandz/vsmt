module Data.Typechecked exposing (..)

{-| Function types, sorts, annotated expressions, and
type checking errors.
-}

import Dict exposing (Dict)
import Data.Parsed exposing (Location)

{-| An expression annotated with sorts.
-}
type ExprT = ExprT String Sort (List ExprT)

getHead : ExprT -> String
getHead (ExprT head _ _) = head

getSort : ExprT -> Sort
getSort (ExprT _ sort _) = sort

getSubs : ExprT -> List ExprT
getSubs (ExprT _ _ subs) = subs

exprTEq : ExprT -> ExprT -> Bool
exprTEq (ExprT hd1 st1 subs1) (ExprT hd2 st2 subs2) =
  if hd1 == hd2 && st1 == st2 then
    List.all identity (List.map2 exprTEq subs1 subs2)
  else False

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

sortToString : Sort -> String
sortToString sort =
  case sort of
    BoolSort -> "Bool"
    RealSort -> "Real"
    UninterpSort s -> s

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