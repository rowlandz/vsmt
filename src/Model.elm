module Model exposing
  ( Model, initialModel
  , TacticSelector
  , makeTacticSelectorsFor
  )

import Array exposing (Array)
import Data.Canvas exposing (Canvas)
import Tactic exposing (Tactic)
import Tactics.All

type alias Model =
  { currentCanvas : Canvas
  , canvasHistory : List Canvas
  , tacticSelectors : Array TacticSelector
  , messagePanelText : String
  }

{-| An entry in the tactic (top-right) panel. -}
type alias TacticSelector =
  { tactic : Tactic
  , args : Array String
  }

initialModel : Model
initialModel =
  { currentCanvas =
      Data.Canvas.MkCEntry
        { uninterpSorts = Array.fromList [ ]
        , variables = Array.fromList [ ("x", "Bool"), ("y", "Bool"), ("z", "Bool") ]
        , expr = "(not (and x (or y (not z))))"
        }
  , canvasHistory = []
  , tacticSelectors = makeTacticSelectorsFor "Entry"
  , messagePanelText = ""
  }

makeTacticSelectorsFor : String -> Array TacticSelector
makeTacticSelectorsFor canvasID =
  Tactics.All.all
  |> List.filter (\tac -> tac.fromCanvas == canvasID)
  |> List.map (\tac -> { tactic = tac, args = Array.repeat (List.length tac.params) "" })
  |> Array.fromList