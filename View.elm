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
    in
        Html.div
            []
            [ Svg.svg
                [ width "1280"
                , height "1280"
                , viewBox "0 0 1280 1280"
                , Html.onMouseEnter LeaveUI
                ]
                [ renderTree model.canvas ]
            , PickerUI.buildUI model
                (List.append PickerUI.swatches
                    [ PickerUI.iconBtn FA.undo canUndo Undo
                    , PickerUI.iconBtn FA.repeat canRedo Redo
                    ]
                )
                Left
            ]


renderTree : Tree Int -> Svg Msg
renderTree t =
    case t of
        Tree.Leaf v ->
            renderLeaf v

        Tree.Node b ne nw se sw ->
            Svg.lazy (Svg.g [])
                (List.map renderTree [ ne, nw, se, sw ])


renderLeaf : Tree.Bounded Int -> Svg Msg
renderLeaf v =
    Svg.rect
        [ x (toString v.boundingBox.pos.x)
        , y (toString v.boundingBox.pos.y)
        , width (toString v.boundingBox.size.width)
        , height (toString v.boundingBox.size.height)
        , stroke "white"
        , fill (PicoPalette.clr v.val)
        ]
        []
