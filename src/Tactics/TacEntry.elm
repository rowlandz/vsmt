module Tactics.TacEntry exposing (parseTopLevelExpr)

import Array
import Dict
import Set exposing (Set)
import Data.Parsed exposing (..)
import Data.Parsed exposing (ParsedExp(..))
import Data.Typechecked exposing (..)
import Func.Parsing exposing (parseExp, parseType)
import Func.Typechecking exposing (typecheckExp)
import Func.ErrorPrinting exposing (tcerrToString)
import Canvas exposing (Canvas(..), CEntry, CTopLevelExpr)
import Tactic exposing (Tactic)

parseTopLevelExpr : Tactic
parseTopLevelExpr =
  { name = "parse top level expression"
  , fromCanvas = "Entry"
  , run = \c -> case c of
      Canvas.MkCEntry entry ->
        validate entry
          |> Result.map MkCTopLevelExpr
          |> Result.mapError (List.intersperse "\n\n" >> String.concat)
      _ -> Err "Wrong canvas type"
  }


validate : CEntry -> Result (List String) CTopLevelExpr
validate entry =
  case validateUninterpSorts (Array.toList entry.uninterpretedSorts) of
    Err errs -> Err errs
    Ok uninterpSorts -> case validateVars uninterpSorts (Array.toList entry.variables) of
      Err errs -> Err errs
      Ok varContext -> case validateMainExpr varContext entry.expr of
        Err errs -> Err errs
        Ok exprT -> Ok { varContext = varContext, expr = exprT }



-- Validate uninterpreted sorts


validateUninterpSorts : List String -> Result (List String) (Set String)
validateUninterpSorts sortNames =
  let (fails, succs) = sortNames |> List.map
        (\sort -> case String.trim sort of
          "Real" -> Err [ "Sort `Real` is already defined." ]
          "Bool" -> Err [ "Sort `Bool` is already defined." ]
          trimedSort -> Ok trimedSort
        ) |> resultsPartition
      moreFails = listFindDuplicates succs |> List.map
        (\sort -> "Sort " ++ sort ++ " is declared twice.")
      allFails = fails ++ moreFails
  in  if List.length allFails == 0 then Ok (Set.fromList succs) else Err allFails


listFindDuplicates : List a -> List a
listFindDuplicates l = case l of
  [] -> []
  x :: ls -> (if listContains x ls then [x] else []) ++ listFindDuplicates ls

listContains : a -> List a -> Bool
listContains x l = case l of
  [] -> False
  y :: ls -> if x == y then True else listContains x ls



-- Parse and valiate variables and their types


validateVars : Set String -> List ( String, String ) -> Result (List String) VarContext
validateVars sorts vars =
  case vars |> List.map Tuple.first |> listFindDuplicates of
    d::dups -> Err (d::dups |> List.map (\x -> "Variable " ++ x ++ " is defined multiple times."))
    [] -> case vars |> List.map (Tuple.second >> parseType) |> resultsPartition of
      (f::ails, _) -> Err (f::ails)
      ([], parsedTypes) -> case parsedTypes |> List.map (parsedTypeToFuncType sorts) |> resultsPartition of
        (f::ails, _) -> Err (f::ails)
        ([], funcTypes) -> Ok (buildVarContext (List.map2 (\v t -> (Tuple.first v, t)) vars funcTypes))

parsedTypeToFuncType : Set String -> ParsedType -> Result (List String) FuncType
parsedTypeToFuncType uninterpSorts pt =
  let (fails1, paramSorts) = pt.paramSorts |> List.map (parsedSortToSort uninterpSorts) |> resultsPartition
  in  case pt.retSort |> parsedSortToSort uninterpSorts of
        Err fail2 -> Err (fails1 ++ fail2)
        Ok retSort ->
          if List.length paramSorts == List.length pt.paramSorts then
            Ok { paramSorts = paramSorts, retSort = retSort }
          else Err fails1

parsedSortToSort : Set String -> ParsedSort -> Result (List String) Sort
parsedSortToSort uninterpSorts parsedSort =
  case parsedSort.name of
    "Bool" -> Ok BoolSort
    "Real" -> Ok RealSort
    other ->
      if Set.member other uninterpSorts then Ok (UninterpSort other)
      else Err [ "Sort " ++ other ++ " has not been declared." ]

buildVarContext : List ( String, FuncType ) -> VarContext
buildVarContext varDecls =
  let (fv, ff) = varDecls |> List.partition (\(_, ty) -> List.length ty.paramSorts == 0)
      fv1 = fv |> List.map (Tuple.mapSecond .retSort)
  in { freeVars = Dict.fromList fv1, freeFuns = Dict.fromList ff }



-- Parse and type check the main expression


validateMainExpr : VarContext -> String -> Result (List String) ExprT
validateMainExpr ctx exprString =
  case parseExp exprString of
    Err parseErr -> Err [ parseErr ]
    Ok parsedExp -> typecheckExp ctx parsedExp |> Result.mapError (List.map (tcerrToString exprString))

resultsPartition : List (Result (List a) b) -> ( List a, List b )
resultsPartition results =
  case results of
    [] -> ([], [])
    (Err errs :: t) ->
      let (es, ss) = resultsPartition t
      in  (errs ++ es, ss)
    (Ok succ :: t) ->
      let (es, ss) = resultsPartition t
      in  (es, succ :: ss)