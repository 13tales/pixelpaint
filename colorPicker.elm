module ColorPicker exposing (picker)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import PicoPalette


picker : Int -> Html msg
picker selectedClr =
    div
        [ style
            [ ( "display", "flex" )
            , ( "position", "fixed" )
            , ( "bottom", "2em" )
            ]
        ]
        (List.map (swatch selectedClr) (List.range 1 16))


swatch : Int -> Int -> Html msg
swatch selectedClr swatchClr =
    let
        borderClr =
            if selectedClr == swatchClr then
                ( "border-color", "#FFFFFF" )
            else
                ( "border-color", "#000000" )
    in
        div
            [ style
                [ ( "background-color", PicoPalette.clr swatchClr )
                , ( "height", "3em" )
                , ( "width", "3em" )
                , ( "display", "flex" )
                , ( "border-radius", "50%" )
                , borderClr
                , ( "border", "solid 0.1em" )
                , ( "margin", ".1em" )
                ]
            , onClick (ColorSelection swatchClr)
            ]
            []
