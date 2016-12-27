module Types exposing (..)

import Mouse exposing (Position)
import Tree exposing (Tree)
import Array exposing (Array)
import Window exposing (Size)
import SelectionList exposing (SelectionList)


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
    , treeDebugOutline : Bool
    , currentTool : Tool
    , brushSize : SelectionList BrushSize
    }


type alias Drag =
    { start : Position
    , current : Position
    }


type Msg
    = DragStart Position
    | DragAt Position
    | DragEnd Position
    | ColorSelection Int
    | Undo
    | Redo
    | EnterUI
    | LeaveUI
    | ToggleDebug
    | ChooseTool Tool
    | CycleBrushSize
    | NoOp


type UIposition
    = Top
    | Left
    | Bottom
    | Right


type Tool
    = SquareBrush
    | CircleBrush


type BrushSize
    = One
    | Four
    | Eight
    | Sixteen
    | ThirtyTwo


brushSizeToInt : BrushSize -> Int
brushSizeToInt size =
    case size of
        One ->
            1

        Four ->
            4

        Eight ->
            8

        Sixteen ->
            16

        ThirtyTwo ->
            32



{- type alias SelectionList a =
       { previous : List a
       , selected : a
       , next : List a
       }


   cycleSelection : SelectionList a -> SelectionList a
   cycleSelection s =
       { s
       | previous = List.append s.previous s.selected
       , selected = Maybe.withDefault
-}
