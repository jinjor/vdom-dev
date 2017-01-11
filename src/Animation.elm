port module Animation exposing (..)

import Time exposing (..)
import AltHtml exposing (..)
import Spinner exposing (..)


main : Program Never Model Msg
main = program
  { init = init
  , update = update
  , view = view
  , subscriptions = \model ->
    Sub.batch
      [ Time.every (2 * second) ToggleColor
      , Sub.map SpinnerMsg (Spinner.subscriptions model.spinner)
      ]
  }


type alias Model =
  { spinner : Spinner
  , colored : Bool
  }


init : (Model, Cmd Msg)
init =
  { spinner = Tuple.first Spinner.init
  , colored = True
  } ! [ Cmd.map SpinnerMsg (Tuple.second Spinner.init) ]


type Msg
  = SpinnerMsg Spinner.Msg
  | ToggleColor Time


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SpinnerMsg msg ->
      let
        (spinner, cmd) =
          Spinner.update msg model.spinner
      in
        { model | spinner = spinner } ! [ Cmd.map SpinnerMsg cmd ]

    ToggleColor _ ->
      { model | colored = not (model.colored) } ! []


view : Model -> Html Msg
view model =
  div []
    [ AltHtml.map SpinnerMsg <| Spinner.view 100 model.colored model.spinner
    ]
