module Func.ErrorPrinting exposing (tcerrToString)

import Data.Parsed exposing (Location)
import Data.Typechecked exposing (TCErr(..))



tcerrToString : String -> TCErr -> String
tcerrToString source err =
  let (header, loc1) = case err of
        UndeclaredSymbol loc -> ("Undeclared symbol", loc)
        SortMismatch loc _ _ -> ("Sort mismatch", loc)
        ArityMismatch loc ob ex ->
          ("Arity mismatch: Expected " ++ String.fromInt ex ++ " but found " ++ String.fromInt ob, loc)
        UnrecognizedHeadSymbol loc -> ("Unrecognized head", loc)
        UnknownExpressionForm loc -> ("Unknown expression form", loc)
        SortsShouldMatch _ loc _ _ -> ("Sorts should match", loc)
  in  header ++ "\n" ++ underlined source loc1

underlined : String -> Location -> String
underlined source loc =
  let lines = String.lines source
      ( l1, c1 ) = loc.start |> Tuple.mapBoth (\l -> l - 1) (\c -> c - 1)
      ( l2, c2 ) = loc.end |> Tuple.mapBoth (\l -> l - 1) (\c -> c - 1)
  in
      lines |> List.indexedMap
        (\idx line ->
          if idx == l1 && l1 == l2 then
            [ line
            , String.repeat c1 " " ++ String.repeat (c2 - c1) "^"
            ]
          else if idx == l1 then
            [ line
            , String.repeat c1 " " ++ String.repeat ((String.length line) - c1) "^"
            ]
          else if l1 < idx && idx < l2 then
            [ line
            , String.repeat (String.length line) "^"
            ]
          else if idx == l2 then
            [ line
            , String.repeat c2 "^"
            ]
          else
            [ line ]
        ) |> List.concat |> List.intersperse "\n" |> String.concat