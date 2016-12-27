module PickerUI exposing (..)

import Html exposing (..)
import Html.Attributes as Html
import Html.Events as Html
import Svg exposing (Svg)
import Svg.Attributes as Svg exposing (..)
import Svg.Events
import Types exposing (..)
import Color exposing (Color)
import Array
import FontAwesome as FA
import PicoPalette
import List
import SelectionList exposing (SelectionList)


buildUI : Model -> List (Model -> Html Msg) -> UIposition -> Html Msg
buildUI model controls position =
    let
        containerStandard =
            [ ( "display", "flex" )
            , ( "position", "fixed" )
            , ( "align-items", "center" )
            ]

        pickerStandard =
            [ ( "display", "flex" )
            , ( "flex-wrap", "wrap" )
            , ( "background-color", "black" )
            , ( "border", "0.1em solid white" )
            , ( "border-radius", "2em" )
            , ( "padding", "1ex" )
            ]

        pickerStyle =
            List.append pickerStandard <|
                if position == Bottom || position == Top then
                    [ ( "flex-direction", "row" ) ]
                else
                    [ ( "flex-direction", "column" ) ]

        containerStyle =
            List.append containerStandard <|
                case position of
                    Bottom ->
                        [ ( "flex-direction", "column" )
                        , ( "bottom", "1em" )
                        , ( "height", "auto" )
                        , ( "width", "100%" )
                        ]

                    Top ->
                        [ ( "flex-direction", "column" )
                        , ( "top", "1em" )
                        , ( "height", "auto" )
                        , ( "width", "100%" )
                        ]

                    Left ->
                        [ ( "flex-direction", "row" )
                        , ( "left", "1em" )
                        , ( "top", "1em" )
                        , ( "width", "auto" )
                        , ( "min-height", "6em" )
                        , ( "height", "100%" )
                        ]

                    Right ->
                        [ ( "flex-direction", "row" )
                        , ( "right", "1em" )
                        , ( "top", "1em" )
                        , ( "width", "auto" )
                        , ( "min-height", "4em" )
                        , ( "height", "100%" )
                        ]
    in
        div
            [ Html.style containerStyle
            ]
            [ div
                [ Html.style pickerStyle
                , Html.onMouseEnter EnterUI
                ]
                (reverseMap controls model)
            ]


reverseMap : List (a -> b) -> a -> List b
reverseMap fList arg =
    let
        inner f a r =
            case f of
                [] ->
                    r

                h :: t ->
                    inner t a (List.append r <| [ h a ])
    in
        inner fList arg []


iconBtn : (Color -> Int -> Html Msg) -> Bool -> Msg -> Model -> Html Msg
iconBtn element isActive msg model =
    span
        [ Html.style [ ( "margin", ".5ex" ) ]
        , Html.onClick msg
        ]
        [ element (btnColour isActive) 48 ]


btnColour : Bool -> Color
btnColour active =
    if active == True then
        Color.white
    else
        Color.darkCharcoal


swatches : List (Model -> Html Msg)
swatches =
    List.map colourSwatch (List.range 1 16)


colourSwatch : Int -> Model -> Html Msg
colourSwatch swatchClr model =
    let
        border =
            if model.selectedClr == swatchClr then
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
            , Html.onClick (ColorSelection swatchClr)
            ]
            []


squareBrush : Model -> Html Msg
squareBrush model =
    Svg.svg [ width "48", height "48" ]
        [ Svg.rect [ width "32", height "32", stroke "white", fill "black" ]
            []
        ]


brushSizePicker : Msg -> Model -> Html Msg
brushSizePicker msg model =
    Html.div
        [ Html.style
            [ ( "width", "48px" )
            , ( "height", "48px" )
            , ( "display", "flex" )
            , ( "flex-direction", "row" )
            , ( "justify-content", "center" )
            , ( "align-items", "flex-end" )
            , ( "flex-wrap", "wrap" )
            , ( "padding-bottom", "1.5ex" )
            ]
        , Html.onClick msg
        ]
        [ div
            []
            [ FA.paint_brush Color.white 20
            , FA.expand Color.white 20
            , span
                [ Html.style
                    [ ( "font-family", "futura" )
                    , ( "font-size", "10pt" )
                    , ( "color", "white" )
                    , ( "display", "block" )
                    ]
                ]
                [ text <| (toString <| brushSizeToInt model.brushSize.selected) ++ " pix"
                ]
            ]
        , div [] <| selectionDisplay model.brushSize
        ]


selectionCircle : Bool -> Html Msg
selectionCircle isSelected =
    case isSelected of
        True ->
            FA.circle Color.white 8

        False ->
            FA.circle_o Color.white 8


selectionDisplay : SelectionList a -> List (Html Msg)
selectionDisplay s =
    let
        f =
            (\v ->
                if v == s.selected then
                    True
                else
                    False
            )
                >> selectionCircle
    in
        SelectionList.map f s |> SelectionList.toList
