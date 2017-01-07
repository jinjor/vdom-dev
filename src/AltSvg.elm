module AltSvg exposing (..)

import AltHtml as Html

import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import VirtualDom exposing (..)


type alias Svg msg =
  VirtualDom.Node msg


svg : List (Html.Attribute msg) -> List (Svg msg) -> Html.Html msg
svg =
  node "svg"


circle : List (Attribute msg) -> List (Svg msg) -> Svg msg
circle =
  node "circle"


line : List (Attribute msg) -> List (Svg msg) -> Svg msg
line =
  node "line"



svgNamespace : Attribute msg
svgNamespace =
  VirtualDom.property "namespace" (E.string "http://www.w3.org/2000/svg")


{-| Create any SVG node. To create a `<rect>` helper function, you would write:
    rect : List (Attribute msg) -> List (Svg msg) -> Svg msg
    rect attributes children =
        node "rect" attributes children
You should always be able to use the helper functions already defined in this
library though!
-}
node : String -> List (Attribute msg) -> List (Svg msg) -> Svg msg
node name =
  \attributes children ->
    VirtualDom.node name (svgNamespace :: attributes) children

---

type alias Attribute msg = VirtualDom.Property msg


x1 : String -> Attribute msg
x1 =
  attribute "x1"


{-|-}
x2 : String -> Attribute msg
x2 =
  attribute "x2"


y1 : String -> Attribute msg
y1 =
  attribute "y1"


{-|-}
y2 : String -> Attribute msg
y2 =
  attribute "y2"


cx : String -> Attribute msg
cx =
  attribute "cx"


{-|-}
cy : String -> Attribute msg
cy =
  attribute "cy"


stroke : String -> Attribute msg
stroke =
  attribute "stroke"


{-|-}
fill : String -> Attribute msg
fill =
  attribute "fill"


r : String -> Attribute msg
r =
  attribute "r"


width : String -> Attribute msg
width =
  attribute "width"


viewBox : String -> Attribute msg
viewBox =
  attribute "viewBox"
