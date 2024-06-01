module Parsed exposing
  ( ParsedExp(..), parseExp
  , ParsedType, parseType
  , ParsedSort
  , Location
  )

{-| Contains parsers and data structures for the things the
user inputs as text. Namely `ParsedExp` for the main expression
and `ParsedType` for the types of variables.
-}

import Parser exposing
  ( Parser, (|.), (|=)
  , backtrackable, chompIf, chompWhile, getChompedString, getPosition
  , lazy, oneOf, sequence, spaces, succeed, token
  )



type alias Location =
  { start : ( Int, Int )
  , end : ( Int, Int )
  }

symbol : Parser String
symbol = getChompedString (chompIf isSymbolChar |. chompWhile isSymbolChar)

isSymbolChar : Char -> Bool
isSymbolChar c =
  not (List.member c ['(', ')', '[', ']', ' ', '\n', '\t', '\r'])



-- ParsedExp


type ParsedExp
  = SSymb Location String
  | SList Location (List ParsedExp)

parseExp : String -> Result String ParsedExp
parseExp s =
  Parser.run
    (succeed identity
      |. spaces
      |= exp
      |. spaces
    ) s
    |> Result.mapError Parser.deadEndsToString

exp : Parser ParsedExp
exp =
  oneOf
    [ succeed (\b s e -> SSymb { start = b, end = e } s)
        |= getPosition
        |= symbol
        |= getPosition
    , succeed (\b l e -> SList { start = b, end = e } l)
        |= getPosition
        |= sequence
            { start = "("
            , separator = ""
            , end = ")"
            , spaces = spaces
            , item = lazy (\_ -> exp)
            , trailing = Parser.Forbidden
            }
        |= getPosition
    ]



-- ParsedType


type alias ParsedType =
  { paramSorts : List ParsedSort
  , retSort : ParsedSort
  , loc : Location
  }

type alias ParsedSort =
  { name : String
  , loc : Location
  }
  
parseType : String -> Result String ParsedType
parseType s =
  Parser.run
    (succeed identity
      |. spaces
      |= funcType
      |. spaces
    ) s
    |> Result.mapError Parser.deadEndsToString

funcType : Parser ParsedType
funcType =
  oneOf
    [ succeed (\b p r e -> { paramSorts = p, retSort = r, loc = { start = b, end = e } })
        |= getPosition
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
        |= getPosition
    , succeed
        (\b p r e -> case r of
          Just s -> { paramSorts = [p], retSort = s, loc = { start = b, end = e } }
          Nothing -> { paramSorts = [], retSort = p, loc = { start = b, end = e } }
        )
        |= getPosition
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
        |= getPosition
    ]

sort : Parser ParsedSort
sort =
  succeed (\b s e -> { name = s, loc = { start = b, end = e } })
    |= getPosition
    |= symbol
    |= getPosition