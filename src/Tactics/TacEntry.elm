module Tactics.TacEntry exposing (parseTopLevelExpr)

import Array
import Dict
import Set exposing (Set)
import Data.Canvas as Canvas exposing (Canvas(..), CEntry, CTopLevelExpr)
import Data.Parsed exposing (..)
import Data.Parsed exposing (ParsedExp(..))
import Data.Typechecked exposing (..)
import Func.Parsing exposing (parseExp, parseType)
import Func.Typechecking exposing (typecheckExp)
import Func.ErrorPrinting exposing (tcerrToString)
import Common exposing (allMustSucceed, andCheck, andCheckL, andCheckRL, resultBind, yielding)
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
  resultBind (validateUninterpSorts (Array.toList entry.uninterpSorts))  <| \uninterpSorts ->
  resultBind (validateVars uninterpSorts (Array.toList entry.variables)) <| \varContext ->
  resultBind (validateMainExpr varContext entry.expr)                    <| \exprT ->
  Ok { varContext = varContext, expr = exprT }



-- Validate uninterpreted sorts


validateUninterpSorts : List String -> Result (List String) (Set String)
validateUninterpSorts usrInp =
  let trimmedInp = List.map String.trim usrInp in
  allMustSucceed
    (trimmedInp |> List.map
      (\sortName -> case sortName of
        "Real" -> Err [ "Sort `Real` is already defined." ]
        "Bool" -> Err [ "Sort `Bool` is already defined." ]
        _ -> Ok ()
      )
    )
  |> andCheck
    (allMustSucceed
      (listFindDuplicates trimmedInp |> List.map
        (\sort -> Err [ "Sort " ++ sort ++ " is declared twice." ])
      )
    )
  |> yielding (Set.fromList trimmedInp)


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
validateVars declaredSorts varInps =
  allMustSucceed
      (varInps |> List.map Tuple.first |> listFindDuplicates |> List.map
        (\dup -> Err [ "Variable " ++ dup ++ " is declared multiple times." ])
      )
  |> andCheckL (allMustSucceed (List.map (Tuple.second >> parseType) varInps))
  |> Result.andThen
      (\parsedTypes -> allMustSucceed (List.map (parsedTypeToFuncType declaredSorts) parsedTypes))
  |> Result.map
      (\funcTypes -> buildVarContext (List.map2 (\v t -> (Tuple.first v, t)) varInps funcTypes))

parsedTypeToFuncType : Set String -> ParsedType -> Result (List String) FuncType
parsedTypeToFuncType declaredSorts pt =
  allMustSucceed (List.map (parsedSortToSort declaredSorts) pt.paramSorts)
  |> andCheckRL (parsedSortToSort declaredSorts pt.retSort)
  |> Result.map (\(x, y) -> { paramSorts = x, retSort = y })

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