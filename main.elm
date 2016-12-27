module Main exposing (..)

import Html exposing (..)
import Html.Attributes as Html
import Html.Events as Html
import Tree exposing (Tree(..))
import Maybe exposing (..)
import Mouse exposing (Position)
import Window exposing (Size)
import Array exposing (Array)
import Types exposing (..)
import View exposing (view)
import Keyboard exposing (KeyCode)
import Char
import Debug
import Tuple
import SelectionList exposing (SelectionList)


{- TODO:
   - Add size change UI for square brush
   - Change brush selection visuals to avoid conflict with undo/redo semantics
   - Implement round brush
   - Add flood fill
   - Implement variable canvas sizing
-}


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
        }


brushSizeList : SelectionList BrushSize
brushSizeList =
    SelectionList.fromList One [ Four, Eight, Sixteen, ThirtyTwo ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        DragStart xy ->
            ( { model
                | drag =
                    (Just (Drag xy xy))
                , canvas = addTile model xy
              }
            , Cmd.none
            )

        DragAt xy ->
            ( { model
                | drag = (Maybe.map (\{ start } -> Drag start xy) model.drag)
                , canvas = addTile model xy
              }
            , Cmd.none
            )

        DragEnd _ ->
            ( dragEndHelper model
            , Cmd.none
            )

        ColorSelection n ->
            ( { model
                | selectedClr = n
              }
            , Cmd.none
            )

        Undo ->
            ( historyHelper model 1
            , Cmd.none
            )

        Redo ->
            ( historyHelper model (-1)
            , Cmd.none
            )

        EnterUI ->
            ( { model | mouseOverUI = True }
            , Cmd.none
            )

        LeaveUI ->
            ( { model | mouseOverUI = False }
            , Cmd.none
            )

        ToggleDebug ->
            ( { model | treeDebugOutline = not model.treeDebugOutline }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )

        ChooseTool t ->
            ( { model | currentTool = t }, Cmd.none )

        CycleBrushSize ->
            ( { model | brushSize = SelectionList.cycleSelection model.brushSize }
            , Cmd.none
            )


addTile : Model -> Position -> Tree Int
addTile model point =
    let
        maxDepth =
            maxTreeDepth model (brushSizeToInt model.brushSize.selected)

        fCollision =
            case model.currentTool of
                SquareBrush ->
                    pointInRegion

                CircleBrush ->
                    pointInRegion
    in
        insertHelper maxDepth fCollision ( point, model.selectedClr ) model.canvas


insertHelper : (Tree.BoundingBox -> Bool) -> (Position -> Tree.BoundingBox -> Bool) -> ( Position, a ) -> Tree a -> Tree a
insertHelper depth fCollision ( pos, clr ) tree =
    case tree of
        Leaf l ->
            let
                collidesWithBrush =
                    fCollision pos (Debug.log "Checking box: " l.boundingBox)
            in
                if (depth l.boundingBox) && (collidesWithBrush) then
                    if clr /= l.val then
                        Leaf { l | val = clr }
                    else
                        tree
                else if not (collidesWithBrush) then
                    tree
                else
                    insertHelper depth fCollision ( pos, clr ) (Tree.subDivide tree)

        Node box ne nw se sw ->
            if not (fCollision pos box) then
                tree
            else
                (Node box
                    (insertHelper depth fCollision ( pos, clr ) ne)
                    (insertHelper depth fCollision ( pos, clr ) nw)
                    (insertHelper depth fCollision ( pos, clr ) se)
                    (insertHelper depth fCollision ( pos, clr ) sw)
                )


maxTreeDepth : Model -> Int -> Tree.BoundingBox -> Bool
maxTreeDepth model brushSize box =
    let
        tileSize =
            getTileSize model

        ( targetWidth, targetHeight ) =
            ( (Tuple.first tileSize) * brushSize
            , (Tuple.second tileSize) * brushSize
            )
    in
        box.size.width == targetWidth || box.size.height == targetHeight


saveHistory : Array (Tree Int) -> Tree Int -> Array (Tree Int)
saveHistory history tree =
    let
        stateToBeAdded =
            Array.fromList [ tree ]
    in
        if Array.length history >= 50 then
            Array.append stateToBeAdded <| Array.slice 0 -1 history
        else
            Array.append stateToBeAdded history


historyHelper : Model -> Int -> Model
historyHelper model step =
    let
        newIndex =
            model.historyIndex + step

        newState =
            Array.get newIndex model.history
    in
        case newState of
            Nothing ->
                model

            Just t ->
                { model
                    | canvas = t
                    , historyIndex = newIndex
                }


dragEndHelper : Model -> Model
dragEndHelper model =
    let
        newCanvas =
            Tree.cullTree model.canvas

        resetHistory =
            Array.slice model.historyIndex (Array.length model.history) model.history
    in
        case model.historyIndex of
            0 ->
                { model
                    | drag = Nothing
                    , canvas = newCanvas
                    , history = saveHistory model.history newCanvas
                }

            _ ->
                { model
                    | drag = Nothing
                    , canvas = newCanvas
                    , history = saveHistory resetHistory newCanvas
                    , historyIndex = 0
                }



-- Helper functions


tileAtCursor : Position -> Model -> ( Int, Int )
tileAtCursor pos model =
    let
        x =
            toFloat pos.x

        y =
            toFloat pos.y

        columns =
            toFloat <| model.columns

        rows =
            toFloat <| model.rows

        ( width, height ) =
            getTileSize model
    in
        ( truncate <| (x / toFloat width) * columns
        , truncate <| (y / toFloat height) * rows
        )


getTileSize : Model -> ( Int, Int )
getTileSize model =
    ( model.window.width // model.columns, model.window.height // model.rows )


boxInRegion : Tree.BoundingBox -> Tree.BoundingBox -> Bool
boxInRegion newTile region =
    (newTile.pos.x + newTile.size.width)
        <= (region.pos.x + region.size.width)
        && newTile.pos.x
        >= region.pos.x
        && (newTile.pos.y + newTile.size.height)
        <= (region.pos.y + region.size.height)
        && newTile.pos.y
        >= region.pos.y


pointInRegion : Position -> Tree.BoundingBox -> Bool
pointInRegion cursorPos region =
    Debug.log "x match: "
        (cursorPos.x
            >= region.pos.x
            && cursorPos.x
            <= (region.pos.x + region.size.width)
        )
        && Debug.log "y match: "
            (cursorPos.y
                >= region.pos.y
                && cursorPos.y
                <= (region.pos.y + region.size.height)
            )


intersectSquare : Model -> Position -> Tree.BoundingBox -> Bool
intersectSquare model cursorPos boxToCheck =
    let
        ( tileWidth, tileHeight ) =
            getTileSize model

        brushSize =
            brushSizeToInt model.brushSize.selected

        ( xOffset, yOffset ) =
            ( (brushSize // 2) * tileWidth, (brushSize // 2) * tileHeight )

        ( boxWidth, boxHeight ) =
            ( brushSize * tileWidth, brushSize * tileHeight )

        brushBox =
            Debug.log "brushbox: " (Tree.newBox (cursorPos.x - xOffset) (cursorPos.y - yOffset) boxWidth boxHeight)

        brushCentre =
            Tree.boxCentre brushBox

        boxCentre =
            Tree.boxCentre boxToCheck
    in
        Debug.log "checkresult: "
            (abs (boxCentre.x - brushCentre.x)
                * 2
                < brushBox.size.width
                + boxToCheck.size.width
                && abs (boxCentre.y - brushCentre.y)
                * 2
                < brushBox.size.height
                + boxToCheck.size.height
            )


debugKey : KeyCode
debugKey =
    Char.toCode '`'



-- SUBSCRIPTIONS


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
                    ]
            else
                Sub.none

        Just _ ->
            Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]
