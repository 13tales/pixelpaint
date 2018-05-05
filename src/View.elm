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

        brushEffect =
            if (model.brushSize.selected == One || model.drag /= Nothing || model.mouseOverUI) then
                Debug.log "no brush effect" (div [] [])
            else
                Debug.log "Yay, brush effect" (renderCursor model.brushArea)
    in
        Html.div
            []
            [ Svg.svg
                [ width "1280"
                , height "1280"
                , viewBox "0 0 1280 1280"
                , Html.onMouseEnter LeaveUI
                , cursor "crosshair"
                ]
                [ renderTree model.treeDebugOutline model.canvas
                , brushEffect
                ]
            , panel PickerUI.swatches Bottom
            , panel
                [ {- PickerUI.iconBtnChangeable FA.square_o FA.square (isActiveTool SquareBrush) (ChooseTool SquareBrush)
                     , PickerUI.iconBtnChangeable FA.circle_o FA.circle (isActiveTool CircleBrush) (ChooseTool CircleBrush)
                     ,
                  -}
                  PickerUI.brushSizePicker CycleBrushSize
                , PickerUI.iconBtnGreyable FA.step_backward canUndo Undo
                , PickerUI.iconBtnGreyable FA.step_forward canRedo Redo
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


renderCursor : Tree.BoundingBox -> Svg Msg
renderCursor box =
    Svg.rect
        [ x (toString box.pos.x)
        , y (toString box.pos.y)
        , width (toString box.size.width)
        , height (toString box.size.height)
        , stroke "white"
        , fill "none"
        ]
        []


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
