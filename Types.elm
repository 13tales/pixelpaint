module Types exposing (..)

import Mouse exposing (Position)
import Tree exposing (Tree)
import Array exposing (Array)
import Window exposing (Size)


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


type Msg
    = DragStart Position
    | DragAt Position
    | DragEnd Position
    | ColorSelection Int
    | Undo
    | Redo
    | EnterUI
    | LeaveUI


type UIposition
    = Top
    | Left
    | Bottom
    | Right
