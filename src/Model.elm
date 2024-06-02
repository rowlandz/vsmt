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
        { uninterpSorts = Array.fromList [ ]
        , variables = Array.fromList [ ("x", "Bool"), ("y", "Bool"), ("z", "Bool") ]
        , expr = "(not (and x (or y (not z))))"
        }
  , canvasHistory = []
  , messagePanelText = ""
  }