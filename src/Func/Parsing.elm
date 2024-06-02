module Func.Parsing exposing
  ( parseExp
  , parseType
  , parseSymbol
  )

{-| Contains parsers for user input, namely for expressions,
function types, and symbols.
-}

import Parser exposing
  ( Parser, (|.), (|=)
  , backtrackable, chompIf, chompWhile, getChompedString, getOffset
  , lazy, oneOf, sequence, spaces, succeed, token
  )
import Data.Parsed exposing (..)



parseSymbol : String -> Result String String
parseSymbol =
  Parser.run
    (succeed identity
      |. spaces
      |= symbol
      |. spaces
    ) >> Result.mapError Parser.deadEndsToString

symbol : Parser String
symbol = getChompedString (chompIf isSymbolChar |. chompWhile isSymbolChar)

isSymbolChar : Char -> Bool
isSymbolChar c =
  not (List.member c ['(', ')', '[', ']', ' ', '\n', '\t', '\r'])



-- ParsedExp


parseExp : String -> Result String ParsedExp
parseExp =
  Parser.run
    (succeed identity
      |. spaces
      |= exp
      |. spaces
    ) >> Result.mapError Parser.deadEndsToString

exp : Parser ParsedExp
exp =
  oneOf
    [ succeed (\b s e -> SSymb { start = b, end = e } s)
        |= getOffset
        |= symbol
        |= getOffset
    , succeed (\b l e -> SList { start = b, end = e } l)
        |= getOffset
        |= sequence
            { start = "("
            , separator = ""
            , end = ")"
            , spaces = spaces
            , item = lazy (\_ -> exp)
            , trailing = Parser.Forbidden
            }
        |= getOffset
    ]



-- ParsedType

{-| Parse a sort or function type like one of these:

    Real
    Real -> Bool
    (Real Real) -> Bool
-}
parseType : String -> Result (List String) ParsedType
parseType =
  Parser.run
    (succeed identity
      |. spaces
      |= funcType
      |. spaces
    ) >> Result.mapError (Parser.deadEndsToString >> List.singleton)

funcType : Parser ParsedType
funcType =
  oneOf
    [ succeed (\b p r e -> { paramSorts = p, retSort = r, loc = { start = b, end = e } })
        |= getOffset
        |= sequence
            { start = "("
            , separator = ""
            , end = ")"
            , spaces = spaces
            , item = sort
            , trailing = Parser.Forbidden
            }
        |. spaces
        |. token "->"
        |. spaces
        |= sort
        |= getOffset
    , succeed
        (\b p r e -> case r of
          Just s -> { paramSorts = [p], retSort = s, loc = { start = b, end = e } }
          Nothing -> { paramSorts = [], retSort = p, loc = { start = b, end = e } }
        )
        |= getOffset
        |= sort
        |= backtrackable
            (oneOf
              [ succeed Just
                  |. spaces
                  |. token "->"
                  |. spaces
                  |= sort
              , succeed Nothing
              ]
            )
        |= getOffset
    ]

sort : Parser ParsedSort
sort =
  succeed (\b s e -> { name = s, loc = { start = b, end = e } })
    |= getOffset
    |= symbol
    |= getOffset