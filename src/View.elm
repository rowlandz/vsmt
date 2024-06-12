module View exposing (view)

import Array exposing (Array)
import Dict
import Element exposing (Element, el, text, row, column, fill, width, height, rgb255, padding, paddingXY)
import Element.Background as Bg
import Element.Font as Font
import Element.Input as Input
import Element.Border as Border
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Common exposing (listIndexedMap2)
import Data.Canvas as Canvas exposing (..)
import Data.Typechecked exposing (ExprT, exprTToString)
import Event exposing (Event(..))
import ExampleEntries
import Model exposing (Model, TacticSelector)

view : Model -> Html Event
view model = Element.layout [] (mainElement model)

mainElement : Model -> Element Event
mainElement model =
  column
    [ width fill, height fill
    , Font.family [ Font.sansSerif ], Font.size 14, Font.color white
    , Bg.color grey2
    , Element.clip
    ]
    [ headerStrip
    , row [ width fill, height fill ]
        [ canavsHistory model
        , column [ width fill, height fill ]
            [ tacticPanel model.tacticSelectors
            , messageBox model
            ]
        ]
    ]
    
headerStrip : Element Event
headerStrip =
  row
    [ width fill
    , padding 5
    , Border.widthEach { bottom=1, left=0, right=0, top=0 }
    , Border.color grey3
    ]
    [ text "Select an example: "
    , exampleSelector
    , Input.button attrsButton { label = text "undo", onPress = Just UserClickedUndo }
    ]

exampleSelector : Element Event
exampleSelector =
  Element.html
    (Html.select
      [ Html.Attributes.style "padding" "5px"
      , Html.Attributes.style "border-width" "1px"
      , Html.Attributes.style "border-radius" "5px"
      , Html.Attributes.style "border-color" "#aaaaaa"
      , Html.Attributes.style "background-color" "#282828"
      , Html.Attributes.style "color" "#aaaaaa"
      ]
      (Dict.keys ExampleEntries.examples |> List.map
        (\example ->
          Html.option [ Html.Events.onClick (UserSelectedExample example) ]
            [ Html.text example ]
        )
      )
    )



-- Tactic Panel


tacticPanel : Array TacticSelector -> Element Event
tacticPanel tacSelectors =
  column
    [ width fill, height fill
    , padding 5
    , Border.widthEach { bottom=1, left=0, right=0, top=0 }
    , Border.color grey3
    ]
    (List.indexedMap tacticSelector (Array.toList tacSelectors))

tacticSelector : Int -> TacticSelector -> Element Event
tacticSelector tacIdx tacSelector =
  row [ width fill ]
    (Input.button attrsButton
      { onPress = Just (UserClickedTactic tacSelector)
      , label = text "██"
      }
    :: el [ paddingXY 5 0 ] (text tacSelector.tactic.name)
    :: listIndexedMap2 (tacticSelectorParam tacIdx) tacSelector.tactic.params (Array.toList tacSelector.args)
    )

tacticSelectorParam : Int -> Int -> String -> String -> Element Event
tacticSelectorParam tacIdx paramIdx name value  =
  el [ paddingXY 10 0 ]
    (Input.text (attrsCodeInput ++ [ width (Element.px 80) ])
      { text = value
      , onChange = Event.UserChangedTacticArg tacIdx paramIdx
      , label = Input.labelLeft [] (text (name ++ " = "))
      , placeholder = Just (Input.placeholder [] (text "x"))
      }
    )



-- Message Panel


messageBox : Model -> Element Event
messageBox model =
  el
    [ width fill, height fill
    , padding 5
    , Font.family [ Font.monospace ]
    ]
    (text model.messagePanelText)



---


canavsHistory : Model -> Element Event
canavsHistory model =
  column
    [ width fill, height (fill |> Element.maximum 900)
    , padding 10
    , Border.widthEach { bottom=0, left=0, right=1, top=0 }
    , Border.color grey3
    , Element.scrollbars
    ]
    (viewCanvas model.currentCanvas :: List.map viewCanvas model.canvasHistory)

viewCanvas : Canvas -> Element Event
viewCanvas canvas =
  el [ padding 5, width fill ]
    (el
      [ padding 5
      , width fill
      , Border.width 1, Border.color grey3, Border.rounded 5
      ]
      (case canvas of
        Canvas.MkCEntry c -> cEntry c
        Canvas.MkCTopLevelExpr c -> cTopLevelExpr c
        Canvas.MkCDPLL c -> cDPLL c
        Canvas.MkCUnsat -> cUnsat
      )
    )



-- CEntry


cEntry : CEntry -> Element Event
cEntry canvas =
  column [ width fill ]
    [ sortEntries canvas.uninterpSorts
    , varEntries canvas.variables
    , el [ width fill, padding 10 ]
        (Input.multiline (width fill :: attrsCodeInput)
          { onChange = Event.UserChangedText
          , text = canvas.expr
          , label = Input.labelAbove [ Font.family [ Font.sansSerif ] ] (text "Enter Expression")
          , placeholder = Just (Input.placeholder [] (text "(and x y)"))
          , spellcheck = False
          }
        )
    ]

sortEntries : Array String -> Element Event
sortEntries sorts =
  column [ padding 10 ]
    [ text "Sorts"
    , column []
        (Array.toList (sorts |> Array.indexedMap sortEntry))
    , Input.button attrsButton
        { onPress = Just Event.UserAddedSort
        , label = text "+"
        }
    ]

sortEntry : Int -> String -> Element Event
sortEntry idx sortName =
  row [ paddingXY 0 2 ]
    [ Input.text attrsCodeInput
        { onChange = Event.UserChangedSortName idx
        , text = sortName
        , label = Input.labelHidden ("sort" ++ String.fromInt idx)
        , placeholder = Just (Input.placeholder [] (text "S"))
        }
    , Input.button [ padding 5 ]
        { onPress = Just (Event.UserDeletedSort idx)
        , label = text "Delete"
        }
    ]

varEntries : Array ( String, String ) -> Element Event
varEntries vars =
  column [ padding 10 ]
    [ text "Variables"
    , column []
        (Array.toList (vars |> Array.indexedMap varEntry))
    , Input.button attrsButton
        { onPress = Just Event.UserAddedVar
        , label = text "+"
        }
    ]

varEntry : Int -> ( String, String ) -> Element Event
varEntry idx (varName, varType) =
  row [ paddingXY 0 2 ]
    [ Input.text attrsCodeInput
        { onChange = Event.UserChangedVarName idx
        , text = varName
        , label = Input.labelHidden ("var" ++ String.fromInt idx)
        , placeholder = Just (Input.placeholder [] (text "x"))
        }
    , el [ paddingXY 4 0 ] (text ":")
    , Input.text attrsCodeInput
        { onChange = Event.UserChangedVarType idx
        , text = varType
        , label = Input.labelHidden ("varType" ++ String.fromInt idx)
        , placeholder = Just (Input.placeholder [] (text "Bool"))
        }
    , Input.button attrsButton
        { onPress = Just (Event.UserDeletedVar idx)
        , label = text "Delete"
        }
    ]



-- CTopLevelExpr


cTopLevelExpr : CTopLevelExpr -> Element Event
cTopLevelExpr canvas =
  el [ Font.family [ Font.monospace ] ]
    (Element.paragraph [] [ text (exprTToString canvas.expr) ])



-- CDPLL


cDPLL : CDPLL -> Element Event
cDPLL c =
  column [ width fill, height fill ]
    [ el [ Border.widthEach { bottom=1, left=0, right=0, top=0 } ]
      (text "DPLL")
    , case c.branches of
        [] -> text "No branches!"
        [ branch ] -> cDPLLBranch branch
        branch :: rest ->
          column []
            [ cDPLLBranch branch
            , text (String.fromInt (List.length rest) ++ " more branch(es)")
            ]
    , cDPLLBoundVars c.boundVars
    ]

cDPLLBranch : DPLLBranch -> Element Event
cDPLLBranch branch =
  column [ padding 5, Font.family [ Font.monospace ] ]
    (   cDPLLPartialSolution branch.partialSol
     :: List.map cDPLLClause branch.clauses
    )

cDPLLClause : DPLLClause -> Element Event
cDPLLClause clause =
  el [ padding 5 ]
    (text ("(or " ++ (String.concat (List.intersperse " " (List.map cDPLLAtom clause))) ++ ")"))


cDPLLAtom : DPLLAtom -> String
cDPLLAtom { get, negated } =
  if negated then "(not " ++ get ++ ")" else get

cDPLLBoundVars : List ( String, ExprT ) -> Element Event
cDPLLBoundVars binds =
  column [ Font.family [ Font.monospace ] ]
    (binds |> List.map
      (\(v, e) -> text (v ++ " := " ++ exprTToString e))
    )

cDPLLPartialSolution : List DPLLAtom -> Element Event
cDPLLPartialSolution atoms =
  row []
    [ text "Partial Solution: "
    , text ("(and " ++ String.concat (List.intersperse " " (List.map cDPLLAtom atoms)) ++ ")")
    ]
  



-- CUnsat


cUnsat : Element Event
cUnsat = text "UNSAT"



-- Shared


attrsCodeInput : List (Element.Attribute msg)
attrsCodeInput =
  [ padding 5
  , Font.family [ Font.monospace ], Font.color white
  , Bg.color grey2
  , Border.width 1, Border.color grey3, Border.rounded 5
  ]

attrsButton : List (Element.Attribute msg)
attrsButton =
  [ padding 5
  , Font.family [ Font.sansSerif ], Font.color grey4
  , Element.mouseOver [ Font.color white, Border.color white ]
  , Bg.color grey2
  , Border.width 1, Border.color grey4, Border.rounded 5
  ]

-- grey1 = rgb255 0x11 0x11 0x11
grey2 : Element.Color
grey2 = rgb255 0x28 0x28 0x28
grey3 : Element.Color
grey3 = rgb255 0x44 0x44 0x44
grey4 : Element.Color
grey4 = rgb255 0xaa 0xaa 0xaa
white : Element.Color
white = rgb255 0xff 0xff 0xff