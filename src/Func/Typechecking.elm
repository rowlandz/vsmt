module Func.Typechecking exposing (typecheckExp)

{-| Typechecking functions.
-}

import Dict
import Common exposing (allMustSucceed, andCheck, yielding)
import Data.Parsed exposing (Location, ParsedExp, ParsedExp(..), getLoc)
import Data.Typechecked exposing (Sort(..), ExprT(..), TCErr(..), VarContext, getSort)

typecheckExp : VarContext -> ParsedExp -> Result (List TCErr) ExprT
typecheckExp ctx pe =
  case pe of

    SSymb loc x ->
      case Dict.get x ctx.freeVars of
        Just xSort -> Ok (ExprT x xSort [])
        Nothing -> case String.toInt x of
          Just _ -> Ok (ExprT x RealSort [])
          Nothing -> Err [ UnknownExpressionForm loc ]

    SList _ (SSymb fLoc f :: args) ->
      let argLocs = List.map getLoc args in
      allMustSucceed (List.map (typecheckExp ctx) args)
      |> Result.map (\argEs -> List.map2 Tuple.pair argEs argLocs)
      |> Result.andThen (typecheckApp ctx f fLoc)

    SList loc _ -> Err [ UnknownExpressionForm loc ]

typecheckApp : VarContext -> String -> Location -> List ( ExprT, Location ) -> Result (List TCErr) ExprT
typecheckApp ctx f fLoc largs =
  let args = List.map Tuple.first largs in

  if f == "and" || f == "or" then
    allMustSucceed (List.map (sortMustBe BoolSort) largs)
    |> Result.map (ExprT f BoolSort)
  
  else if f == "=>" || f == "implies" then
    allMustSucceed (List.map (sortMustBe BoolSort) largs)
    |> andCheck (arityMustBe 2 fLoc args)
    |> yielding (ExprT f BoolSort args)

  else if f == "not" then
    allMustSucceed (List.map (sortMustBe BoolSort) largs)
    |> andCheck (arityMustBe 1 fLoc args)
    |> yielding (ExprT f BoolSort args)

  else if f == "neg" then
    allMustSucceed (List.map (sortMustBe RealSort) largs)
    |> andCheck (arityMustBe 1 fLoc args)
    |> yielding (ExprT f RealSort args)

  else if f == "+" || f == "*" then
    allMustSucceed (List.map (sortMustBe RealSort) largs)
    |> Result.map (ExprT f RealSort)

  else if f == "-" || f == "/" then
    allMustSucceed (List.map (sortMustBe RealSort) largs)
    |> andCheck (arityMustBe 2 fLoc args)
    |> yielding (ExprT f RealSort args)

  else if f == "<" || f == "<=" || f == ">" || f == ">=" then
    allMustSucceed (List.map (sortMustBe RealSort) largs)
    |> andCheck (arityMustBe 2 fLoc args)
    |> yielding (ExprT f BoolSort args)

  else if f == "=" || f == "distinct" then
    sortsShouldMatch largs
    |> Result.map (\_ -> ExprT f BoolSort args)

  else if f == "if" || f == "ite" then
    case largs of
      condition :: thenBranch :: elseBranch :: [] ->
        sortMustBe BoolSort condition
        |> andCheck (sortsShouldMatch [ thenBranch, elseBranch ])
        |> yielding (ExprT f (getSort (Tuple.first thenBranch)) args)
      _ -> Err [ ArityMismatch fLoc (List.length largs) 3 ]

  else
    case Dict.get f ctx.freeFuns of
      Just funcType ->
        allMustSucceed (List.map2 sortMustBe funcType.paramSorts largs)
        |> andCheck (arityMustBe (List.length funcType.paramSorts) fLoc args)
        |> yielding (ExprT f funcType.retSort args)
      Nothing -> Err [ UnrecognizedHeadSymbol fLoc ]

sortMustBe : Sort -> ( ExprT, Location ) -> Result (List TCErr) ExprT
sortMustBe sort ( e, loc ) =
  let eSort = getSort e in
  if sort == eSort then Ok e else Err [ SortMismatch loc eSort sort ]

arityMustBe : Int -> Location -> List a
  -> Result (List TCErr) (List a)
arityMustBe arity fLoc args =
  if List.length args == arity then Ok args
  else Err [ ArityMismatch fLoc (List.length args) arity ]

sortsShouldMatch : List ( ExprT, Location ) -> Result (List TCErr) (Maybe ( Sort, Location ))
sortsShouldMatch exprs =
  case exprs of
    [] -> Ok Nothing
    e :: es -> case sortsShouldMatch es of
      Err errs -> Err errs
      Ok Nothing -> Ok (Just (Tuple.mapFirst getSort e))
      Ok (Just (sort, loc)) ->
        if getSort (Tuple.first e) == sort then Ok (Just ( sort, Tuple.second e ))
        else Err [ SortsShouldMatch (getSort (Tuple.first e)) (Tuple.second e) sort loc ]