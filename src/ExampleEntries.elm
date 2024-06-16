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
    , Tuple.pair "DPLL example"
      { uninterpSorts = Array.empty
      , variables = Array.fromList [ ( "a", "Bool" ), ( "b", "Bool" ), ( "c", "Bool" ), ( "d", "Bool" ) ]
      , expr = "(and\n"
            ++ "  (or (not a) b c)\n"
            ++ "  (or a c d)\n"
            ++ "  (or a c (not d))\n"
            ++ "  (or a (not c) d)\n"
            ++ "  (or a (not c) (not d))\n"
            ++ "  (or (not b) (not c) d)\n"
            ++ "  (or (not a) b (not c))\n"
            ++ "  (or (not a) (not b) c)\n"
            ++ ")"
      }
    , Tuple.pair "EUF+LRA"
      { uninterpSorts = Array.empty
      , variables = Array.fromList [ ( "x", "Real" ), ( "y", "Real" ), ( "f", "Real -> Real" ) ]
      , expr = "(and\n"
            ++ "  (>= x y)\n"
            ++ "  (<= x y)\n"
            ++ "  (not (= (f (+ x 1)) (f (+ y 1))))\n"
            ++ ")"
      }
    ]