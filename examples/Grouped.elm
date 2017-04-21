module Main exposing (main)

import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Svg exposing (Svg, Attribute)
import Svg.Attributes as Attributes exposing (fill, stroke, strokeWidth)
import Grouped exposing (..)
import Axis exposing (..)
import Colors exposing (..)


data : List (List Float)
data =
  [ [ 2, 3, 1 ], [ 5, 1, 4 ], [ 1, 5, 3 ] ]


colors : List (List (Attribute msg))
colors =
  [ [ stroke transparent ]
  , [ stroke transparent, fill blueFill ]
  , [ stroke transparent, fill "lightpink" ]
  ]


group : Int -> List Float -> Group msg
group index data =
  { bars = List.map2 (Bar ) colors data
  , label = "Disease no. " ++ toString index
  }


independentAxis : IndependentAxis
independentAxis =
  { position = \min max -> min
  , line = Nothing
  , marks = \_ -> List.map independentMark [ 0, 1, 2, 3, 4, 5 ]
  , mirror = False
  }


independentMarkView : Float -> IndependentMarkView
independentMarkView position =
  { grid = Nothing
  , junk = Just (fullLine [ stroke "white", strokeWidth "2px" ])
  , tick = Just simpleTick
  , label = Just (simpleLabel position)
  }


independentMark : Float -> IndependentMark
independentMark position =
  { position = position
  , view = independentMarkView position
  }


main : Html msg
main =
  div [ style [ ("padding", "40px" ) ] ]
    [ viewCustom
        { dependentAxis =
            { line = Just simpleLine
            , mark =
                { label = stringLabel
                , tick = Just simpleTick
                }
            }
        , independentAxis = independentAxis
        }
        { toGroups = List.indexedMap group
        , width = 0.9
        }
        data
    ]


viewCircle : Svg msg
viewCircle =
  Svg.circle
    [ Attributes.r "5"
    , Attributes.stroke "transparent"
    , Attributes.fill "pink"
    ]
    []