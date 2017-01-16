module Internal.Scatter exposing (..)

import Svg
import Svg.Attributes
import Plot.Types exposing (..)
import Internal.Types exposing (..)
import Internal.Draw exposing (..)


type alias Config a =
    { style : Style
    , customAttrs : List (Svg.Attribute a)
    , radius : Int
    }


defaultConfig : Config a
defaultConfig =
    { style = [ ( "fill", "transparent" ) ]
    , customAttrs = []
    , radius = 5
    }


view : Meta -> Config a -> List Point -> Svg.Svg a
view meta { style, radius } points =
    let
        svgPoints =
            List.map meta.toSvgCoords points
    in
        Svg.g
            [ Svg.Attributes.style (toStyle style)
            , Svg.Attributes.clipPath ("url(#" ++ toClipPathId meta ++ ")")
            ]
            (List.map (toSvgCircle radius) svgPoints)


toSvgCircle : Int -> Point -> Svg.Svg a
toSvgCircle radius ( x, y ) =
    Svg.circle
        [ Svg.Attributes.cx (toString x)
        , Svg.Attributes.cy (toString y)
        , Svg.Attributes.r (toString radius)
        ]
        []
