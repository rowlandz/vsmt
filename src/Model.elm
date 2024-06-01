module Model exposing
  ( Model, initialModel
  )

import Array
import Canvas exposing (Canvas)

type alias Model =
  { currentCanvas : Canvas
  , canvasHistory : List Canvas
  , messagePanelText : String
  }

initialModel : Model
initialModel =
  { currentCanvas =
      Canvas.MkCEntry
        { uninterpretedSorts = Array.fromList [ "S" ]
        , variables = Array.fromList [ ("x", "Real"), ("y", "Real"), ("z", "Real") ]
        , expr = "(and (= x y) (= y z) (not (= x z)))"
        }
  , canvasHistory = []
  , messagePanelText = ""
  }