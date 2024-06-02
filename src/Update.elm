module Update exposing (..)

import Array
import Event exposing (..)
import Model exposing (..)
import Canvas exposing (..)
import Tactics.All
import Tactic exposing (Tactic)

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

    UserClickedTactic tacName ->
      case findTacticByName tacName Tactics.All.all of
        Just found -> case found.run model.currentCanvas of
          Ok newCanvas -> { model | currentCanvas = newCanvas, canvasHistory = model.currentCanvas :: model.canvasHistory }
          Err err -> model |> withMsg err
        Nothing -> model |> withMsg ("Could not find tactic with name " ++ tacName)

withMsg : String -> Model -> Model
withMsg msg model = { model | messagePanelText = msg }

findTacticByName : String -> List Tactic -> Maybe Tactic
findTacticByName name tactics =
  case tactics of
    [] -> Nothing
    t :: ts -> if t.name == name then Just t else findTacticByName name ts

arrayDeleteNth : Int -> Array.Array a -> Array.Array a
arrayDeleteNth i arr =
  Array.append
    (Array.slice 0 i arr)
    (Array.slice (i+1) (Array.length arr) arr)