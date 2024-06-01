module Tactics.TacEntry exposing (parseTopLevelExpr)

import Dict exposing (Dict)
import Parsed exposing (..)
import Parsed exposing (ParsedExp(..))
import ExprT exposing (..)
import Canvas exposing (Canvas, CEntry)
import Tactic exposing (Tactic)

parseTopLevelExpr : Tactic
parseTopLevelExpr =
  { name = "parse top level expression"
  , fromCanvas = "Entry"
  , run = \c -> case c of
      Canvas.MkCEntry entry ->
        Err "Unimplemented"
      _ -> Err "Wrong canvas type"
  }

type TCErr
  = UndeclaredSymbol Location
  | SortMismatch Location Sort Sort
  | ArityMismatch Location Int Int
  | UnrecognizedHeadSymbol Location
  | UnknownExpressionForm Location


typecheckExp :
  { freeVars : Dict String Sort
  , freeFuns : Dict String FuncType
  } -> ParsedExp -> Result (List TCErr) ExprT
typecheckExp ctx pe =
  case pe of
    SSymb loc x ->
      case Dict.get x ctx.freeVars of
        Just xSort -> Ok (ExprT x xSort [])
        Nothing -> Err [ UndeclaredSymbol loc ]
    SList _ (SSymb headLoc head :: tail) ->
      let tailResults = List.map (typecheckExp ctx) tail
          (fails, succs) = resultsPartition tailResults
      in  if List.length succs == List.length tailResults then
            case typecheckApplication head succs (List.map getLoc tail) headLoc of
              Ok app -> Ok app
              Err errs -> Err (errs ++ fails)
          else Err fails
    SList loc _ -> Err [ UnknownExpressionForm loc ]

getLoc : ParsedExp -> Location
getLoc pe = case pe of
  SSymb loc _ -> loc
  SList loc _ -> loc

typecheckApplication : String -> List ExprT -> List Location -> Location -> Result (List TCErr) ExprT
typecheckApplication f args argLocs headLoc =
  if f == "and" || f == "or" then
    let results = List.map2
          (\arg loc -> case getSort arg of
            BoolSort -> Ok arg
            sort -> Err [ SortMismatch loc sort BoolSort ]
          ) args argLocs
        (fails, succs) = resultsPartition results
    in  if List.length succs == List.length results
          then Ok (ExprT f BoolSort args)
        else Err fails
  else if f == "not" then
    case ( args , argLocs ) of
      ( arg :: [], loc :: [] ) ->
        case getSort arg of
          BoolSort -> Ok (ExprT "not" BoolSort [ arg ])
          sort -> Err [ SortMismatch loc sort BoolSort ]
      _ -> Err [ ArityMismatch headLoc (List.length args) 1 ]
  else if f == "+" || f == "-" || f == "*" || f == "/" then
    let results = List.map2
          (\arg loc -> case getSort arg of
            RealSort -> Ok arg
            sort -> Err [ SortMismatch loc sort RealSort ]
          ) args argLocs
        (fails, succs) = resultsPartition results
    in  if List.length succs == List.length results
          then Ok (ExprT f RealSort args)
        else Err fails
  else
    Err [ UnrecognizedHeadSymbol headLoc ]

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