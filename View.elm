module View exposing (..)

import Html exposing (..)
import Html.Attributes as Html
import Html.Events as Html
import Svg exposing (Svg)
import Svg.Attributes as Svg exposing (..)
import Svg.Events
import Svg.Lazy as Svg
import Svg.Keyed as Keyed
import PickerUI
import Types exposing (..)
import Tree exposing (Tree(..))
import PicoPalette
import List
import FontAwesome as FA
import Array


view : Model -> Html Msg
view model =
    let
        canUndo =
            ((Array.length model.history) - (model.historyIndex + 1)) > 0

        canRedo =
            model.historyIndex > 0

        panel =
            PickerUI.buildUI model

        isActiveTool t =
            if model.currentTool == t then
                True
            else
                False
    in
        Html.div
            []
            [ Svg.svg
                [ width "1280"
                , height "1280"
                , viewBox "0 0 1280 1280"
                , Html.onMouseEnter LeaveUI
                ]
                [ renderTree model.treeDebugOutline model.canvas ]
            , panel PickerUI.swatches Bottom
            , panel
                [ PickerUI.iconBtn FA.square (isActiveTool SquareBrush) (ChooseTool SquareBrush)
                , PickerUI.iconBtn FA.circle (isActiveTool CircleBrush) (ChooseTool CircleBrush)
                , PickerUI.brushSizePicker CycleBrushSize
                , PickerUI.iconBtn FA.step_backward canUndo Undo
                , PickerUI.iconBtn FA.step_forward canRedo Redo
                ]
                Left
            ]


renderTree : Bool -> Tree Int -> Svg Msg
renderTree debug t =
    case t of
        Tree.Leaf v ->
            renderLeaf debug v

        Tree.Node b ne nw se sw ->
            Svg.lazy (Svg.g [])
                (List.map (renderTree debug) [ ne, nw, se, sw ])


renderLeaf : Bool -> Tree.Bounded Int -> Svg Msg
renderLeaf debug v =
    let
        ifDebug =
            if debug == True then
                "white"
            else
                "none"
    in
        Svg.rect
            [ x (toString v.boundingBox.pos.x)
            , y (toString v.boundingBox.pos.y)
            , width (toString v.boundingBox.size.width)
            , height (toString v.boundingBox.size.height)
            , stroke ifDebug
            , fill (PicoPalette.clr v.val)
            ]
            []
