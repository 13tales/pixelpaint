module Main exposing (..)

import Html exposing (..)
import Html.Attributes as Html
import Html.Events as Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events as Svg exposing (..)
import Svg.Lazy as Svg exposing (..)
import Svg.Keyed as Keyed exposing (..)
import Tree exposing (Tree(..), Bounded)
import Array exposing (..)
import Dict exposing (..)
import Maybe exposing (..)
import Mouse exposing (Position)
import Debug exposing (log)
import Task exposing (..)
import Window exposing (Size)
import Debug exposing (..)
import PicoPalette
import FontAwesome as FA
import Color
import Array exposing (Array)


{- TODO:
   - Grey out undo/redo buttons when there's no past/future history
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
        }



-- MODEL


type alias Model =
    { columns : Int
    , rows : Int
    , canvas : Tree Int
    , selectedClr : Int
    , drag : Maybe Drag
    , history : Array (Tree Int)
    , historyIndex : Int
    , window : Size
    , mouseOverUI : Bool
    }


type alias Drag =
    { start : Position
    , current : Position
    }



-- UPDATE


type Msg
    = DragStart Position
    | DragAt Position
    | DragEnd Position
    | ColorSelection Int
    | Undo
    | Redo
    | EnterUI
    | LeaveUI


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


newPixelHelper : Model -> Position -> Bounded Int
newPixelHelper model pos =
    let
        ( col, row ) =
            tileAtCursor pos model

        ( tileWidth, tileHeight ) =
            ( model.window.width // model.columns, model.window.height // model.rows )
    in
        Tree.setBounds (Tree.newBox pos.x pos.y tileWidth tileHeight) model.selectedClr


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



-- VIEW


view : Model -> Html Msg
view model =
    Html.div
        []
        [ Svg.svg
            [ width "1280"
            , height "1280"
            , viewBox "0 0 1280 1280"
            , Html.onMouseEnter LeaveUI
            ]
            [ renderTree model.canvas ]
        , picker model
        ]


renderTree : Tree Int -> Svg Msg
renderTree t =
    case t of
        Leaf v ->
            renderLeaf v

        Node b ne nw se sw ->
            Svg.lazy (Svg.g [])
                (List.map renderTree [ ne, nw, se, sw ])


renderLeaf : Bounded Int -> Svg Msg
renderLeaf v =
    Svg.rect
        [ x (toString v.boundingBox.pos.x)
        , y (toString v.boundingBox.pos.y)
        , width (toString v.boundingBox.size.width)
        , height (toString v.boundingBox.size.height)
          --, stroke "white"
        , fill (PicoPalette.clr v.val)
        ]
        []


picker : Model -> Html Msg
picker model =
    div
        [ Html.style
            [ ( "display", "flex" )
            , ( "position", "fixed" )
            , ( "bottom", "2em" )
            , ( "background-color", "black" )
            , ( "border", "0.1em solid white" )
            , ( "border-radius", "2em" )
            , ( "padding", "1ex" )
            ]
        , Html.onMouseEnter EnterUI
        ]
        [ div [ Html.style [ ( "display", "flex" ) ] ]
            (List.map (swatch model.selectedClr) (List.range 1 16))
        , div [ Html.style [ ( "display", "flex" ), ( "padding", "1ex" ) ] ]
            [ undoBtn model
            , redoBtn model
            ]
        ]


undoBtn : Model -> Html Msg
undoBtn model =
    let
        undoColour =
            if ((Array.length model.history) - (model.historyIndex + 1)) > 0 then
                Color.white
            else
                Color.darkCharcoal
    in
        span
            [ Html.style [ ( "margin-right", ".5ex" ) ]
            , onClick Undo
            ]
            [ FA.undo undoColour 42 ]


redoBtn : Model -> Html Msg
redoBtn model =
    let
        redoColour =
            if model.historyIndex > 0 then
                Color.white
            else
                Color.darkCharcoal
    in
        span
            [ Html.style [ ( "margin-left", ".5ex" ) ]
            , onClick Redo
            ]
            [ FA.repeat redoColour 42 ]


swatch : Int -> Int -> Html Msg
swatch selectedClr swatchClr =
    let
        border =
            if selectedClr == swatchClr then
                ( "border", "0.2em solid white" )
            else
                ( "border", "0.1em solid white" )
    in
        div
            [ Html.style
                [ ( "background-color", PicoPalette.clr swatchClr )
                , ( "height", "3em" )
                , ( "width", "3em" )
                , ( "display", "flex" )
                , ( "border-radius", "50%" )
                , ( "border-color", "white" )
                , border
                , ( "margin", ".3em" )
                ]
            , onClick (ColorSelection swatchClr)
            ]
            []



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
