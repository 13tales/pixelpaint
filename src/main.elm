module Main exposing (..)

import Html exposing (..)
import Tree exposing (Tree(..))
import Maybe exposing (..)
import Mouse exposing (Position)
import Array exposing (Array)
import Types exposing (..)
import View exposing (view)
import Keyboard exposing (KeyCode)
import Char
import SelectionList exposing (SelectionList)
import Update exposing (update)


{- TODO:
   - Implement round brush
   - Add flood fill
   - Implement variable canvas sizing
-}


main : Program Never Model Msg
main =
    Html.program { init = ( init, Cmd.none ), update = update, view = view, subscriptions = subscriptions }


init : Model
init =
    let
        startingCanvas =
            Leaf
                { boundingBox = { pos = { x = 0, y = 0 }, size = { width = 1280, height = 1280 } }
                , val = 1
                }
    in
        { columns = 128
        , rows = 128
        , canvas = startingCanvas
        , selectedClr = 9
        , drag = Nothing
        , history = Array.fromList [ startingCanvas ]
        , historyIndex = 0
        , window = { height = 1280, width = 1280 }
        , mouseOverUI = False
        , treeDebugOutline = False
        , currentTool = SquareBrush
        , brushSize = brushSizeList
        , brushArea = { pos = { x = 0, y = 0 }, size = { width = 20, height = 20 } }
        }


brushSizeList : SelectionList BrushSize
brushSizeList =
    SelectionList.fromList One [ Two, Four, Eight, Sixteen ]

-- SUBSCRIPTIONS

debugKey : KeyCode
debugKey =
    Char.toCode '`'

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            if not model.mouseOverUI then
                Sub.batch
                    [ Mouse.downs DragStart
                    , Keyboard.presses
                        (\p ->
                            if p == debugKey then
                                ToggleDebug
                            else
                                NoOp
                        )
--                    , Mouse.moves TrackMouse
                    ]
            else
                Sub.none

        Just _ ->
            Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]
