module Func.Typechecking exposing (typecheckExp)

{-| Typechecking functions.
-}

import Dict
import Data.Parsed exposing (Location, ParsedExp, ParsedExp(..), getLoc)
import Data.Typechecked exposing (..)

typecheckExp : VarContext -> ParsedExp -> Result (List TCErr) ExprT
typecheckExp ctx pe =
  case pe of

    SSymb loc x ->
      case Dict.get x ctx.freeVars of
        Just xSort -> Ok (ExprT x xSort [])
        Nothing -> Err [ UndeclaredSymbol loc ]

    SList _ (SSymb fLoc f :: tail) ->
      tail |> List.map (typecheckExp ctx) |> allMustSucceed
      |> Result.map (\tailExps -> List.map2 (\e1 e2 -> (e1, getLoc e2)) tailExps tail)
      |> Result.andThen (typecheckApp f fLoc)

    SList loc _ -> Err [ UnknownExpressionForm loc ]

typecheckApp : String -> Location -> List ( ExprT, Location ) -> Result (List TCErr) ExprT
typecheckApp f fLoc args =
  
  if f == "and" || f == "or" then
    args |> List.map (sortMustBe BoolSort) |> allMustSucceed
    |> Result.map (ExprT f BoolSort)
  
  else if f == "not" then
    args |> List.map (sortMustBe BoolSort) |> allMustSucceed
    |> Result.andThen (arityMustBe 1 fLoc)
    |> Result.map (ExprT f BoolSort)

  else if f == "+" || f == "*" then
    args |> List.map (sortMustBe RealSort) |> allMustSucceed
    |> Result.map (ExprT f RealSort)

  else if f == "-" || f == "/" then
    args |> List.map (sortMustBe RealSort) |> allMustSucceed
    |> Result.andThen (arityMustBe 2 fLoc)
    |> Result.map (ExprT f RealSort)

  else if f == "<" || f == "<=" || f == ">" || f == ">=" then
    args |> List.map (sortMustBe RealSort) |> allMustSucceed
    |> Result.andThen (arityMustBe 2 fLoc)
    |> Result.map (ExprT f RealSort)

  else if f == "=" || f == "distinct" then
    args |> sortsShouldMatch
    |> Result.map (\_ -> ExprT f BoolSort (List.map Tuple.first args))

  else if f == "if" || f == "ite" then
    case args of
      condition :: thenBranch :: elseBranch :: [] ->
        [ sortMustBe BoolSort condition |> killResult
        , sortsShouldMatch [ thenBranch, elseBranch ] |> killResult
        ] |> allMustSucceed
        |> Result.map
          (\_ -> ExprT f (getSort (Tuple.first thenBranch))
            [ Tuple.first condition
            , Tuple.first thenBranch
            , Tuple.first elseBranch
            ]
          )
      _ -> Err [ ArityMismatch fLoc (List.length args) 3 ]

  else
    Err [ UnrecognizedHeadSymbol fLoc ]

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

killResult : Result a b -> Result a ()
killResult = Result.map (\_ -> ())

allMustSucceed : List (Result (List err) a) -> Result (List err) (List a)
allMustSucceed results =
  let ( fails, succs ) = resultsPartition results
  in  if List.length succs == List.length results then
        Ok succs
      else Err fails

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