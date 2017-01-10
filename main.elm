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
        , brushArea = { pos = { x = 0, y = 0 }, size = { width = 20, height = 20 } }
        }


brushSizeList : SelectionList BrushSize
brushSizeList =
    SelectionList.fromList One [ Two, Four, Eight, Sixteen ]



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
            let
                box =
                    model.brushArea

                boxSize =
                    box.size

                newBrush =
                    SelectionList.cycleSelection model.brushSize

                toolSize =
                    brushSizeToInt newBrush.selected

                ( boxW, boxH ) =
                    getTileSize model |> (\( w, h ) -> ( w * toolSize, h * toolSize ))

                newSize =
                    { boxSize
                        | width = boxW
                        , height = boxH
                    }

                brushArea =
                    model.brushArea

                newBrushArea =
                    { brushArea | size = newSize }
            in
                ( { model
                    | brushSize = newBrush
                    , brushArea = newBrushArea
                  }
                , Cmd.none
                )

        TrackMouse p ->
            let
                size =
                    brushSizeToInt model.brushSize.selected

                box =
                    model.brushArea

                toolSize =
                    brushSizeToInt model.brushSize.selected

                ( boxW, boxH ) =
                    getTileSize model |> (\( w, h ) -> ( w * toolSize, h * toolSize ))

                newPos =
                    { x = p.x - (boxW // 2)
                    , y = p.y - (boxH // 2)
                    }

                newBox =
                    { box | pos = newPos }
            in
                ( { model | brushArea = newBox }
                , Cmd.none
                )



{- Thoughts on how to improve the square brush at large sizes:

   - Is the tree bounding box equal to or inside the brush bounding box? Just replace it with a leaf of the desired colour.

   - Does it intersect *but* it's not inside?  Subdivide it, then run again.

   - If we've reached the individual pixel level, then check using point-in-region, and don't go any further
-}


addTile : Model -> Position -> Tree Int
addTile model point =
    let
        maxDepth =
            maxTreeDepth model 1

        fCollision =
            squareBrushCheck model
    in
        insertHelper maxDepth fCollision ( point, model.selectedClr ) model.canvas


insertHelper : (Tree.BoundingBox -> Bool) -> (Position -> Tree.BoundingBox -> BoxCollision) -> ( Position, a ) -> Tree a -> Tree a
insertHelper fDepth fCollision ( pos, clr ) tree =
    let
        treeBox =
            Tree.getBox tree

        atTargetDepth =
            fDepth treeBox

        collidesWithBrush =
            fCollision pos treeBox
    in
        case collidesWithBrush of
            None ->
                tree

            InsideOrEqual ->
                newLeaf treeBox clr

            Intersecting ->
                traverseDeeper fDepth fCollision ( pos, clr ) tree


traverseDeeper : (Tree.BoundingBox -> Bool) -> (Position -> Tree.BoundingBox -> BoxCollision) -> ( Position, a ) -> Tree a -> Tree a
traverseDeeper fDepth fCollision brush tree =
    case tree of
        Leaf l ->
            insertHelper fDepth fCollision brush (Tree.subDivide tree)

        Node box ne nw se sw ->
            (Node box
                (insertHelper fDepth fCollision brush ne)
                (insertHelper fDepth fCollision brush nw)
                (insertHelper fDepth fCollision brush se)
                (insertHelper fDepth fCollision brush sw)
            )


squareBrushCheck : Model -> Position -> Tree.BoundingBox -> BoxCollision
squareBrushCheck model pos boxToCheck =
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
            Debug.log "brushbox: " (Tree.newBox (pos.x - xOffset) (pos.y - yOffset) boxWidth boxHeight)
    in
        case model.brushSize.selected of
            One ->
                if pointInBox pos boxToCheck then
                    if maxTreeDepth model 1 boxToCheck then
                        Debug.log "Painting:" InsideOrEqual
                    else
                        Intersecting
                else
                    None

            _ ->
                if (boxInsideOrEqualBox boxToCheck brushBox) == True then
                    InsideOrEqual
                else if (boxesIntersect boxToCheck brushBox) == True then
                    if maxTreeDepth model 1 boxToCheck then
                        InsideOrEqual
                    else
                        Intersecting
                else
                    None


newLeaf : Tree.BoundingBox -> a -> Tree a
newLeaf box val =
    Leaf <| Tree.setBounds box val


cursorBox : Model -> Position -> Tree.BoundingBox
cursorBox model pos =
    let
        ( tileWidth, tileHeight ) =
            getTileSize model

        brushSize =
            (brushSizeToInt model.brushSize.selected)

        ( xOffset, yOffset ) =
            ( (brushSize // 2) * tileWidth, (brushSize // 2) * tileHeight )

        ( boxWidth, boxHeight ) =
            ( brushSize * tileWidth, brushSize * tileHeight )
    in
        (Tree.newBox (pos.x - xOffset) (pos.y - yOffset) boxWidth boxHeight)


maxTreeDepth : Model -> Int -> Tree.BoundingBox -> Bool
maxTreeDepth model pixelMultiplier box =
    let
        tileSize =
            getTileSize model

        ( targetWidth, targetHeight ) =
            ( (Tuple.first tileSize) * pixelMultiplier
            , (Tuple.second tileSize) * pixelMultiplier
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


boxInsideOrEqualBox : Tree.BoundingBox -> Tree.BoundingBox -> Bool
boxInsideOrEqualBox boxA boxB =
    (boxA.pos.x + boxA.size.width)
        <= (boxB.pos.x + boxB.size.width)
        && boxA.pos.x
        >= boxB.pos.x
        && (boxA.pos.y + boxA.size.height)
        <= (boxB.pos.y + boxB.size.height)
        && boxA.pos.y
        >= boxB.pos.y


pointInBox : Position -> Tree.BoundingBox -> Bool
pointInBox cursorPos region =
    (cursorPos.x
        > region.pos.x
        && cursorPos.x
        < (region.pos.x + region.size.width)
    )
        && (cursorPos.y
                > region.pos.y
                && cursorPos.y
                < (region.pos.y + region.size.height)
           )


boxesIntersect : Tree.BoundingBox -> Tree.BoundingBox -> Bool
boxesIntersect boxA boxB =
    let
        centreA =
            Tree.boxCentre boxA

        centreB =
            Tree.boxCentre boxB
    in
        ((boxA.size.width + boxB.size.width) - abs (centreA.x - centreB.x) * 2)
            > 10
            && ((boxA.size.height + boxB.size.height) - abs (centreA.y - centreB.y) * 2)
            > 10


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
                    , Mouse.moves TrackMouse
                    ]
            else
                Sub.none

        Just _ ->
            Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]
