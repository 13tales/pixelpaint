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
        }



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


insertHelper : (Tree.BoundingBox -> Bool) -> ( Position, a ) -> Tree a -> Tree a
insertHelper depth ( pos, clr ) tree =
    case tree of
        Leaf l ->
            if (depth l.boundingBox) && (pointInRegion pos l.boundingBox) then
                if clr /= l.val then
                    Leaf { l | val = clr }
                else
                    tree
            else if not (pointInRegion pos l.boundingBox) then
                tree
            else
                insertHelper depth ( pos, clr ) (Tree.subDivide tree)

        Node box ne nw se sw ->
            if not (pointInRegion pos box) then
                tree
            else
                (Node box (insertHelper depth ( pos, clr ) ne) (insertHelper depth ( pos, clr ) nw) (insertHelper depth ( pos, clr ) se) (insertHelper depth ( pos, clr ) sw))


addTile : Model -> Position -> Tree Int
addTile model point =
    let
        maxDepth =
            maxTreeDepth model
    in
        insertHelper (maxDepth) ( point, model.selectedClr ) model.canvas


maxTreeDepth : Model -> Tree.BoundingBox -> Bool
maxTreeDepth model box =
    let
        ( tileWidth, tileHeight ) =
            ( model.window.width // model.columns, model.window.height // model.rows )
    in
        box.size.width == tileWidth || box.size.height == tileHeight


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

        width =
            toFloat <| model.window.width

        height =
            toFloat <| model.window.height
    in
        ( truncate <| (x / width) * columns
        , truncate <| (y / height) * rows
        )


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
    cursorPos.x
        >= region.pos.x
        && cursorPos.x
        <= (region.pos.x + region.size.width)
        && cursorPos.y
        >= region.pos.y
        && cursorPos.y
        <= (region.pos.y + region.size.height)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            if not model.mouseOverUI then
                Mouse.downs DragStart
            else
                Sub.none

        Just _ ->
            Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]
