module Spinner exposing (..)

import Time exposing (..)
import AltHtml exposing (..)


type alias Spinner =
  { degree : Int
  , inverse : Bool
  }


init : (Spinner, Cmd Msg)
init =
  { degree = 1, inverse = False } ! []


type Msg
  = Rotate Time
  | ToggleRotation


update : Msg -> Spinner -> (Spinner, Cmd Msg)
update msg model =
  case msg of
    Rotate _ ->
      { model |
        degree =
          (model.degree + 4 * (if model.inverse then -1 else 1)) % 360
      } ! []

    ToggleRotation ->
      { model | inverse = not (model.inverse) } ! []


subscriptions : Spinner -> Sub Msg
subscriptions _ =
  Time.every (100 * millisecond) Rotate


view : Int -> Bool -> Spinner -> Html Msg
view top colored model =
  div
    [ style
      [ ("transform", "rotate(" ++ toString model.degree ++ "deg)")
      , ("width", "100px")
      , ("height", "100px")
      , ("position", "absolute")
      , ("left", toString 100 ++ "px")
      , ("top", toString top ++ "px")
      , ("background-color", if colored then "#acd" else "#aaa")
      ]
    , onClick ToggleRotation
    ]
    []
