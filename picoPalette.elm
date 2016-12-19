module PicoPalette exposing (clr)

import Dict exposing (..)


{- Pico-8 inspired palette for my pixel painting app.
   The 16 colors are mapped to integers. Give the clr function an ineger between 1 -16,
   it will return the appropriate color as a string value for use with CSS or SVG
-}


svgPalette : Dict Int String
svgPalette =
    let
        colorList =
            [ ( 1, "#000000" )
              --"Black"
            , ( 2, "#1D2B53" )
              -- "DarkBlue"
            , ( 3, "#7E2553" )
              -- "DarkPurple"
            , ( 4, "#008751" )
              -- "DarkGreen"
            , ( 5, "#AB5236" )
              -- "Brown"
            , ( 6, "#5F574F" )
              -- "DarkGrey"
            , ( 7, "#C2C3C7" )
              -- "LightGrey"
            , ( 8, "#FFF1E8" )
              -- "White"
            , ( 9, "#FF004D" )
              -- "Red"
            , ( 10, "#FFA300" )
              -- "Orange"
            , ( 11, "#FFEC27" )
              -- "Yellow"
            , ( 12, "#00E436" )
              -- "Green"
            , ( 13, "#29ADFF" )
              -- "Blue"
            , ( 14, "#83769C" )
              -- "Indigo"
            , ( 15, "#FF77A8" )
              -- "Pink"
            , ( 16, "#FFCCAA" )
              -- "Peach"
            ]
    in
        Dict.fromList colorList


clr : Int -> String
clr color =
    let
        key =
            toString color

        value =
            Dict.get color svgPalette
    in
        Maybe.withDefault "#FFFFFF" value
