module Update exposing (update)

import Types exposing (..)
import Treeutils exposing (..)
import SelectionList exposing (SelectionList)


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



{-
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

-}
