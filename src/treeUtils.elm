module Treeutils exposing (..)

import Types exposing (..)
import Mouse exposing (Position)
import Tree exposing (Tree(..))
import Array exposing (Array)

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


