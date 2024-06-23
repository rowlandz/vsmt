module View exposing (view)

import Array exposing (Array)
import Dict
import Element exposing (Element, el, text, row, column, fill, width, height, rgb255, padding, paddingXY, spacing)
import Element.Background as Bg
import Element.Font as Font
import Element.Input as Input
import Element.Border as Border
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Common exposing (listIndexedMap2)
import Data.Canvas as Canvas exposing (..)
import Data.Fract as Fract
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
    , Border.widthEach { noBorders | bottom = 1 }
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
    , Border.widthEach { noBorders | bottom = 1 }
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



--- Canvas History


canavsHistory : Model -> Element Event
canavsHistory model =
  column
    [ width fill, height (fill |> Element.maximum 900)
    , padding 10
    , Border.widthEach { noBorders | right = 1 }
    , Border.color grey3
    , Element.scrollbarY
    , Element.clipX
    ]
    (viewCanvasWithBorder True model.topCanvas :: List.map (viewCanvasWithBorder False) model.canvasHistory)

viewCanvasWithBorder : Bool -> Canvas -> Element Event
viewCanvasWithBorder isTop canvas =
  el
    [ padding 5
    , width fill
    , Border.width 1, Border.color grey3, Border.rounded 5
    ]
    (viewCanvas isTop canvas)

viewCanvas : Bool -> Canvas -> Element Event
viewCanvas isTop canvas =
  case canvas of
    Canvas.MkCEntry c -> cEntry c
    Canvas.MkCTopLevelExpr c -> cTopLevelExpr c
    Canvas.MkCCNF1 c -> cCNF1 c
    Canvas.MkCCNF2 c -> cCNF2 c
    Canvas.MkCDPLL c -> cDPLL isTop c
    Canvas.MkCEUF c -> cEUF c
    Canvas.MkCLRA c -> cLRA c
    Canvas.MkCSimplex c -> cSimplex c
    Canvas.MkCUnsat -> cUnsat



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
    , Input.button attrsButton
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



-- CNF


cCNF1 : CCNF1 -> Element Event
cCNF1 cnf =
  column [ width fill, height fill, spacing 10 ]
    [ text "CNF1"
    , cCNFClauseList exprTToString cnf.clauses
    ]

cCNF2 : CCNF2 -> Element Event
cCNF2 cnf =
  column [ width fill, height fill, spacing 10 ]
    [ text "CNF2"
    , cCNFClauseList identity cnf.clauses
    , cCNFBinds cnf.binds
    ]

cCNFClauseList : (a -> String) -> List (List (Atom a)) -> Element Event
cCNFClauseList toString clauses =
  column [ spacing 5, Font.family [ Font.monospace ] ]
    (List.map (cCNFClause toString >> text) clauses)

cCNFClause : (a -> String) -> List (Atom a) -> String
cCNFClause toString =
    List.map (cCNFAtom toString)
 >> List.intersperse " "
 >> String.concat
 >> (\s -> "(or " ++ s ++ ")")

cCNFAtom : (a -> String) -> Atom a -> String
cCNFAtom toString { prop, negated } =
  if negated then "(not " ++ toString prop ++ ")" else toString prop

cCNFBinds : List ( String, ExprT ) -> Element Event
cCNFBinds binds =
  column [ spacing 5, Font.family [ Font.monospace ] ]
    (List.map (\( x, e ) -> text (x ++ " := " ++ exprTToString e)) binds)



-- CDPLL


cDPLL : Bool -> CDPLL -> Element Event
cDPLL isTop dpll =
  column [ width fill, height fill, spacing 5 ]
    [ cDPLLTabs isTop dpll
    , cDPLLMainContent isTop dpll
    ]

cDPLLTabs : Bool -> CDPLL -> Element Event
cDPLLTabs isTop dpll =
  row [ width fill ]
    [ el
        [ padding 5, height fill
        , Border.widthEach { noBorders | bottom = 1 }
        , Border.color grey3
        ]
        (text "DPLL")
    , row []
        (dpll.branches
        |> List.map partialSolution
        |> List.indexedMap (cDPLLTab isTop dpll.activeBranch)
        )
    , el
        [ width fill, height fill, padding 5
        , Font.italic, Font.family [ Font.sansSerif ]
        , Border.widthEach { noBorders | bottom = 1 }
        , Border.color grey3
        ]
        (if dpll.branches == [] then text "no branches" else Element.none)
    ]

cDPLLTab : Bool -> Int -> Int -> List DPLLAtom -> Element Event
cDPLLTab isTop activeTab idx partialSol =
  if isTop then
    Input.button (attrsTab (activeTab == idx))
      { label = cDPLLTabLabel partialSol
      , onPress = Just (UserClickedDPLLTab idx)
      }
  else
    el (attrsTab (activeTab == idx)) (cDPLLTabLabel partialSol)


cDPLLTabLabel : List DPLLAtom -> Element Event
cDPLLTabLabel partialSol =
  case partialSol of
    [] ->  text "⊤"
    _ -> text (String.concat (List.intersperse " " (List.map cDPLLAtomShort partialSol)))

cDPLLShowHideTheoryPropsButton : Bool -> Element Event
cDPLLShowHideTheoryPropsButton show =
  Input.button (attrsButton ++ [ Element.alignBottom, Element.alignRight ])
    { label = text (if show then "hide theory props" else "show theory props")
    , onPress = Just (UserClickedShowHideTheoryProps (not show))
    }

cDPLLMainContent : Bool -> CDPLL -> Element Event
cDPLLMainContent isTop dpll =
  case activeBranch dpll of
    Just (MkSATBranch branch) -> cDPLLSATBranchContent dpll branch
    Just (MkTheoryBranch branch) -> cDPLLTheoryContent isTop branch
    Nothing -> Element.none

cDPLLSATBranchContent : CDPLL -> SATBranch -> Element Event
cDPLLSATBranchContent dpll branch =
  column [ width fill, height fill ]
    [ row [ width fill ]
        [ cDPLLClauseList branch
        , cDPLLShowHideTheoryPropsButton dpll.showTheoryProps
        ]
    , if dpll.showTheoryProps then cDPLLTheoryProps dpll else Element.none
    ]

cDPLLClauseList : SATBranch -> Element Event
cDPLLClauseList branch =
  case branch.clauses of
    [] ->
      el [ Font.italic, Font.family [ Font.sansSerif ] ] (text "no clauses")
    _ :: _ -> 
      column [ spacing 5, Font.family [ Font.monospace ] ]
        (List.map cDPLLClause branch.clauses)

cDPLLClause : DPLLClause -> Element Event
cDPLLClause clause =
  text ("(or " ++ (String.concat (List.intersperse " " (List.map cDPLLAtom clause))) ++ ")")

cDPLLAtom : DPLLAtom -> String
cDPLLAtom { prop, negated } =
  if negated then "(not " ++ prop ++ ")" else prop

cDPLLAtomShort : DPLLAtom -> String
cDPLLAtomShort { prop, negated } =
  if negated then "¬" ++ prop else prop

cDPLLTheoryProps : CDPLL -> Element Event
cDPLLTheoryProps dpll =
  column [ Font.family [ Font.monospace ] ]
    (dpll.theoryProps |> List.map
      (\{ name, expr, theory } -> text (theory ++ ": " ++ name ++ " := " ++ exprTToString expr))
    )

cDPLLTheoryContent : Bool -> TheoryBranch -> Element Event
cDPLLTheoryContent isTop branch =
  column [ width fill, height fill, spacing 5 ]
    [ cDPLLTheoryTabs True branch
    , cDPLLTheoryMainContent isTop branch
    ]

cDPLLTheoryTabs : Bool -> TheoryBranch -> Element Event
cDPLLTheoryTabs isTop branch =
  row [ width fill ]
    [ row []
        (branch.theoryCanvases
        |> List.map getCanvasType
        |> List.indexedMap (cDPLLTheoryTab isTop branch.activeTheory)
        )
    , el
        [ width fill, height fill, padding 5
        , Font.italic, Font.family [ Font.sansSerif ]
        , Border.widthEach { noBorders | bottom = 1 }
        , Border.color grey3
        ]
        (if branch.theoryCanvases == [] then text "no theories" else Element.none)
    ]

cDPLLTheoryTab : Bool -> Int -> Int -> String -> Element Event
cDPLLTheoryTab isTop activeTab idx theoryName =
  if isTop then
    Input.button (attrsTab (activeTab == idx))
      { label = text theoryName
      , onPress = Just (UserClickedTheoryTab idx)
      }
  else
    el (attrsTab (activeTab == idx)) (text theoryName)


cDPLLTheoryMainContent : Bool -> TheoryBranch -> Element Event
cDPLLTheoryMainContent isTop branch =
  case activeTheory branch of
    Just c -> viewCanvas isTop c
    Nothing -> Element.none


-- EUF


cEUF : CEUF -> Element Event
cEUF euf =
  column [ padding 5, spacing 5, Font.family [ Font.monospace ] ]
    (List.map (exprTToString >> text) euf)



-- CLRA


cLRA : CLRA -> Element Event
cLRA lra =
  column [ padding 5, spacing 5, Font.family [ Font.monospace ] ]
    (List.map (exprTToString >> text) lra)

cSimplex : CSimplex -> Element Event
cSimplex simplex =
  column [ Font.family [ Font.monospace ] ]
    ( cSimplexRow simplex.colLabels
   :: List.map (List.map Fract.toString >> cSimplexRow) simplex.tableau
    )

cSimplexRow : List String -> Element Event
cSimplexRow entries =
  entries
  |> List.map (String.slice 0 5 >> String.padRight 5 ' ')
  |> List.intersperse " "
  |> String.concat
  |> text




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

attrsTab : Bool -> List (Element.Attribute msg)
attrsTab active =
  [ padding 5
  , Font.family [ Font.monospace ]
  , Font.color (if active then white else grey4)
  , Element.mouseOver [ Font.color white, Border.color white ]
  , Bg.color grey2
  , Border.widthEach { thinBorders | bottom = if active then 0 else 1 }
  , Border.color (if active then white else grey3)
  , Border.roundEach { topLeft=5, topRight=5, bottomLeft=0, bottomRight=0 }
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

thinBorders : { bottom : Int, left : Int, right : Int, top : Int }
thinBorders = { bottom = 1, left = 1, right = 1, top = 1 }

noBorders : { bottom : Int, left : Int, right : Int, top : Int }
noBorders = { bottom = 0, left = 0, right = 0, top = 0 }