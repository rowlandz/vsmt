module Update exposing (..)

import Array
import Dict
import Event exposing (..)
import Model exposing (..)
import Data.Canvas exposing (..)
import ExampleEntries

update : Event -> Model -> Model
update event model =
  case event of
    
    UserAddedSort ->
      case model.currentCanvas of
        MkCEntry entry ->
          { model | currentCanvas = MkCEntry { entry | uninterpSorts = Array.push "" entry.uninterpSorts } }
        _ -> model

    UserChangedSortName idx newName ->
      case model.currentCanvas of
        MkCEntry entry ->
          let newSorts = Array.set idx newName entry.uninterpSorts
          in  { model | currentCanvas = MkCEntry { entry | uninterpSorts = newSorts } }
        _ -> model

    UserDeletedSort idx ->
      case model.currentCanvas of
        MkCEntry entry ->
          { model | currentCanvas = MkCEntry { entry | uninterpSorts = arrayDeleteNth idx entry.uninterpSorts } }
        _ -> model

    UserAddedVar ->
      case model.currentCanvas of
        MkCEntry entry ->
          { model | currentCanvas = MkCEntry { entry | variables = Array.push ( "", "" ) entry.variables } }
        _ -> model

    UserChangedVarName idx newName ->
      case model.currentCanvas of
        MkCEntry entry ->
          case Array.get idx entry.variables of
            Just (_, varType) ->
              let newVars = Array.set idx (newName, varType) entry.variables
              in  { model | currentCanvas = MkCEntry { entry | variables = newVars } }
            Nothing -> model
        _ -> model

    UserChangedVarType idx newType ->
      case model.currentCanvas of
        MkCEntry entry ->
          case Array.get idx entry.variables of
            Just (varName, _) ->
              let newVars = Array.set idx (varName, newType) entry.variables
              in  { model | currentCanvas = MkCEntry { entry | variables = newVars } }
            Nothing -> model
        _ -> model

    UserChangedText newText ->
      case model.currentCanvas of
        MkCEntry entry ->
          { model | currentCanvas = MkCEntry { entry | expr = newText } }
        _ -> model

    UserDeletedVar idx ->
      case model.currentCanvas of
        MkCEntry entry ->
          { model | currentCanvas = MkCEntry { entry | variables = arrayDeleteNth idx entry.variables } }
        _ -> model

    UserChangedTacticArg tacIdx argIdx value ->
      case Array.get tacIdx model.tacticSelectors of
        Just tacSelector ->
          let newArgs = Array.set argIdx value tacSelector.args
              newTacSel = Array.set tacIdx { tacSelector | args = newArgs } model.tacticSelectors in
              { model | tacticSelectors = newTacSel }
        Nothing -> model |> withMsg "Param not found"

    UserClickedTactic tacticSelector ->
      case tacticSelector.tactic.run (Array.toList tacticSelector.args) model.currentCanvas of
        Ok newCanvas ->
          { currentCanvas = newCanvas
          , canvasHistory = model.currentCanvas :: model.canvasHistory
          , tacticSelectors = makeTacticSelectorsFor (getCanvasType newCanvas)
          , messagePanelText = ""
          }
        Err err -> model |> withMsg err

    UserClickedUndo ->
      case model.canvasHistory of
        [] -> model |> withMsg "Nothing to undo"
        (c :: cs) ->
          { currentCanvas = c
          , canvasHistory = cs
          , tacticSelectors = makeTacticSelectorsFor (getCanvasType c)
          , messagePanelText = ""
          }

    UserSelectedExample example ->
      case Dict.get example ExampleEntries.examples of
        Just centry ->
          { currentCanvas = MkCEntry centry
          , canvasHistory = []
          , tacticSelectors = makeTacticSelectorsFor "Entry"
          , messagePanelText = ""
          }
        Nothing -> model |> withMsg ("Example " ++ example ++ " not found")

    DoNothing -> model



-- Shared

withMsg : String -> Model -> Model
withMsg msg model = { model | messagePanelText = msg }

-- TODO: Array vs. List what is what?
findTacticSelector : String -> List TacticSelector -> Maybe TacticSelector
findTacticSelector name selectors =
  case selectors of
    [] -> Nothing
    t :: ts -> if t.tactic.name == name then Just t else findTacticSelector name ts

arrayDeleteNth : Int -> Array.Array a -> Array.Array a
arrayDeleteNth i arr =
  Array.append
    (Array.slice 0 i arr)
    (Array.slice (i+1) (Array.length arr) arr)