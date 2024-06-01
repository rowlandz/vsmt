module Data.Parsed exposing (..)

{-| Data structures that hold parsed user-input,
namely expressions, function types, sorts, and locations.
-}

{-| A parsed s-expr like `(and x (or y z))`
-}
type ParsedExp
  = SSymb Location String
  | SList Location (List ParsedExp)

getLoc : ParsedExp -> Location
getLoc pe = case pe of
  SSymb loc _ -> loc
  SList loc _ -> loc

{-| A parsed type like one of these:
    
    Real
    Real -> Bool
    (Real Real) -> Bool
-}
type alias ParsedType =
  { paramSorts : List ParsedSort
  , retSort : ParsedSort
  , loc : Location
  }

{-| A parsed sort like `Real` or `S`
-}
type alias ParsedSort =
  { name : String
  , loc : Location
  }

{-| The position of a substring in a multiline block of text.
`start` and `end` are `( row, col )` pairs.
The substring _includes_ the char at `start` and _excludes_ the char at `end`.
Rows and columns are indexed starting with `1`.
-}
type alias Location =
  { start : ( Int, Int )
  , end : ( Int, Int )
  }