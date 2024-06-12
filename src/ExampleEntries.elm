module ExampleEntries exposing (examples)

import Array
import Dict exposing (Dict)
import Data.Canvas exposing (CEntry)

examples : Dict String CEntry
examples =
  Dict.fromList
    [ ( "empty"
      , { uninterpSorts = Array.empty
        , variables = Array.empty
        , expr = ""
        }
      )
    , ( "EUF simple"
      , { uninterpSorts = Array.fromList [ "S", "T" ]
        , variables = Array.fromList
            [ ( "f", "S -> T" )
            , ( "x", "S" )
            , ( "y", "S" )
            ]
        , expr = "(and\n"
              ++ "  (= x y)\n"
              ++ "  (not (= (f x) (f y)))\n"
              ++ ")"
        }
      )
    , ( "absolute value"
      , { uninterpSorts = Array.empty
        , variables = Array.fromList [ ( "x", "Real" ) ]
        , expr = "(not (>= (if (< x 0) (neg x) x) 0))"
        }
      )
    , ( "demorgan's rule"
      , { uninterpSorts = Array.empty
        , variables = Array.fromList [ ( "p", "Bool" ), ( "q", "Bool" ) ]
        , expr = "(not\n"
              ++ "  (= (not (and p q)) (or (not p) (not q)))\n"
              ++ ")"
        }
      )
    ]