module View exposing (view)

import Array exposing (Array)
import Html exposing (Html)
import Element exposing (Element, el, centerX, text, row, column, fill, width, height, rgb255, padding)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Element.Border as Border
import Model exposing (Model)
import Canvas exposing (Canvas)
import Canvas exposing (CEntry)
import Event exposing (Event)
import Tactics.All
import Tactic exposing (Tactic)
import Event exposing (Event(..))
import Canvas exposing (CTopLevelExpr)
import Data.Typechecked exposing (exprTToString)

view : Model -> Html Event
view model = Element.layout [] (mainElement model)

mainElement : Model -> Element Event
mainElement model =
  column [ width fill, height fill ]
    [ headerStrip
    , row [ width fill, height fill ]
        [ canavsHistory model
        , column [ width fill, height fill ]
            [ tacticSelection model
            , messageBox model
            ]
        ]
    ]
    
headerStrip : Element msg
headerStrip =
  row [ width fill, Background.color (rgb255 240 0 245) ]
    [ text "Example: "
    , Input.button [ centerX ] { label = text "Button", onPress = Nothing }
    ]

tacticSelection : Model -> Element Event
tacticSelection _ =
  column [ width fill, height fill ] (List.map tacticButton Tactics.All.all)

tacticButton : Tactic -> Element Event
tacticButton tac =
  Input.button [ Background.color (rgb255 100 150 230) ]
    { onPress = Just (UserClickedTactic tac.name)
    , label = text tac.name
    }

messageBox : Model -> Element Event
messageBox model =
  el [ width fill, height fill, Font.family [ Font.monospace ] ]
    (text model.messagePanelText)

canavsHistory : Model -> Element Event
canavsHistory model =
  column [ width fill, height fill, padding 10 ]
    (viewCanvas model.currentCanvas :: List.map viewCanvas model.canvasHistory)

viewCanvas : Canvas -> Element Event
viewCanvas canvas =
  el
    [ Border.color (rgb255 0 0 0)
    , Border.width 2
    , padding 5
    , width fill
    , Border.rounded 5
    , Background.color (rgb255 200 200 200)
    ]
    (case canvas of
      Canvas.MkCEntry c -> cEntry c
      Canvas.MkCTopLevelExpr c -> cTopLevelExpr c
    )

cEntry : CEntry -> Element Event
cEntry canvas =
  column [  ]
    [ text "Sorts"
    , sortEntries canvas.uninterpretedSorts
    , text "Variables"
    , varEntries canvas.variables
    , Input.button []
        { onPress = Just Event.UserAddedVar
        , label = text "new var"
        }
    , Input.multiline [ Font.family [ Font.monospace ] ]
        { onChange = Event.UserChangedText
        , text = canvas.expr
        , label = Input.labelAbove [ Font.family [ Font.sansSerif ] ] (text "Enter Expression")
        , placeholder = Just (Input.placeholder [] (text "(and x y)"))
        , spellcheck = False
        }
    ]

sortEntries : Array String -> Element Event
sortEntries sorts =
  column [] (Array.toList (sorts |> Array.indexedMap sortEntry))

sortEntry : Int -> String -> Element Event
sortEntry idx sortName =
  Input.text [ Font.family [ Font.monospace ] ]
    { onChange = Event.UserChangedSortName idx
    , text = sortName
    , label = Input.labelHidden ("sort" ++ String.fromInt idx)
    , placeholder = Just (Input.placeholder [] (text "S"))
    }

varEntries : Array ( String, String ) -> Element Event
varEntries vars =
  column [] (Array.toList (vars |> Array.indexedMap varEntry))

varEntry : Int -> ( String, String ) -> Element Event
varEntry idx (varName, varType) =
  row [ Font.family [ Font.monospace ] ]
    [ Input.text []
        { onChange = Event.UserChangedVarName idx
        , text = varName
        , label = Input.labelHidden ("var" ++ String.fromInt idx)
        , placeholder = Just (Input.placeholder [] (text "x"))
        }
    , text ":"
    , Input.text []
        { onChange = Event.UserChangedVarType idx
        , text = varType
        , label = Input.labelHidden ("varType" ++ String.fromInt idx)
        , placeholder = Just (Input.placeholder [] (text "bool"))
        }
    , Input.button []
        { onPress = Just (Event.UserDeletedVar idx)
        , label = text "Delete"
        }
    ]



cTopLevelExpr : CTopLevelExpr -> Element Event
cTopLevelExpr canvas =
  el [ Font.family [ Font.monospace ] ]
    (text (exprTToString canvas.expr))