module ExampleEntries exposing (examples)

import Array
import Dict exposing (Dict)
import Data.Canvas exposing (Canvas(..))
import Data.Fract as Fract

examples : Dict String Canvas
examples =
  Dict.fromList
    [ Tuple.pair "empty"
        (MkCEntry
          { uninterpSorts = Array.empty
          , variables = Array.empty
          , expr = ""
          }
        )
    , Tuple.pair "EUF simple"
        (MkCEntry
          { uninterpSorts = Array.fromList [ "S", "T" ]
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
    , Tuple.pair "absolute value"
        (MkCEntry
          { uninterpSorts = Array.empty
          , variables = Array.fromList [ ( "x", "Real" ) ]
          , expr = "(not (>= (if (< x 0) (neg x) x) 0))"
          }
        )
    , Tuple.pair "demorgan's rule"
        (MkCEntry
          { uninterpSorts = Array.empty
          , variables = Array.fromList [ ( "p", "Bool" ), ( "q", "Bool" ) ]
          , expr = "(not\n"
                ++ "  (= (not (and p q)) (or (not p) (not q)))\n"
                ++ ")"
          }
        )
    , Tuple.pair "DPLL example"
        (MkCEntry
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
        )
    , Tuple.pair "EUF+LRA"
        (MkCEntry
          { uninterpSorts = Array.empty
          , variables = Array.fromList [ ( "x", "Real" ), ( "y", "Real" ), ( "f", "Real -> Real" ) ]
          , expr = "(and\n"
                ++ "  (>= x y)\n"
                ++ "  (<= x y)\n"
                ++ "  (not (= (f (+ x 1)) (f (+ y 1))))\n"
                ++ ")"
          }
        )
    , Tuple.pair "Simple LRA"
        (MkCLRA
          { colLabels = [ "x", "y", "s1", "s2" ]
          , tableau =
              [ [ Fract.mk_ 1 1, Fract.mk_ 1 1, Fract.mk_ 1 1, Fract.mk_ 0 1, Fract.mk_ -2 1 ]
              , [ Fract.mk_ -1 1, Fract.mk_ 0 1, Fract.mk_ 0 1, Fract.mk_ 1 1, Fract.mk_ -5 1 ]
              ]
          }
        )
    ]