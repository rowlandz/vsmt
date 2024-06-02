module Func.ErrorPrinting exposing (tcerrToString)

import Data.Parsed exposing (Location)
import Data.Typechecked exposing (TCErr(..), sortToString)



tcerrToString : String -> TCErr -> String
tcerrToString source err =
  let (header, selections) = case err of
        UndeclaredSymbol loc ->
          ( "Undeclared symbol", [ loc ] )
        SortMismatch loc ob ex ->
          ( "Sort mismatch: Expected " ++ sortToString ex ++ " but found " ++ sortToString ob
          , [ loc ] )
        ArityMismatch loc ob ex ->
          ("Arity mismatch: Expected " ++ String.fromInt ex ++ " but found " ++ String.fromInt ob
          , [ loc ]
          )
        UnrecognizedHeadSymbol loc ->
          ( "Unrecognized head", [ loc ] )
        UnknownExpressionForm loc ->
          ( "Unknown expression form", [ loc ] )
        SortsShouldMatch s1 loc1 s2 loc2 ->
          ( "Sorts should match: " ++ sortToString s1 ++ " and " ++ sortToString s2
          , [ loc1, loc2 ] )
  in  header ++ "\n" ++ (selectedText selections source |> splitOnNewlines |> renderText)

selectedText : List Location -> String -> List ( String, Bool )
selectedText locs str =
  List.foldl
    (\loc s ->
      { acc = s.acc ++
        [ ( String.slice s.here loc.start str, False )
        , ( String.slice loc.start loc.end str, True )
        ]
      , here = loc.end
      }
    )
    { acc = [], here = 0 }
    locs
    |> (\s -> s.acc ++ [ ( String.slice s.here (String.length str) str, False ) ])


splitOnNewlines : List ( String, Bool ) -> List ( String, Bool )
splitOnNewlines frags =
  frags |> List.concatMap
    (\( txt, ul ) ->
      String.lines txt
      |> List.map (\line -> ( line, ul ))
      |> List.intersperse ( "\n", ul )
    )

renderText : List ( String, Bool ) -> String
renderText frags =
  List.foldl
    (\( txt, ul ) s ->
      if txt == "\n" then
        { rendered = s.rendered ++ s.currLine ++ "\n" ++ (if s.includeUnderline then s.currUnderline ++ "\n" else "")
        , currLine = ""
        , currUnderline = ""
        , includeUnderline = False
        }
      else
        { rendered = s.rendered
        , currLine = s.currLine ++ txt
        , currUnderline = s.currUnderline ++ String.repeat (String.length txt) (if ul then "^" else " ")
        , includeUnderline = s.includeUnderline || ul
        }
    )
    { rendered = "", currLine = "", currUnderline = "", includeUnderline = False }
    (frags ++ [ ( "\n", False ) ])
    |> .rendered