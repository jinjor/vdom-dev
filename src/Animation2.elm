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
      [ Time.every (3 * second) (\_ -> ToggleColor 1)
      , Time.every (3 * second) (\_ -> ToggleColor 2)
      , Sub.map (\msg -> SpinnerMsg msg 1) (Spinner.subscriptions model.spinner1.spinner)
      , Sub.map (\msg -> SpinnerMsg msg 2) (Spinner.subscriptions model.spinner2.spinner)
      ]
  }


type alias Model =
  { spinner1 : SpinnerState
  , spinner2 : SpinnerState
  }


type alias SpinnerState =
  { spinner : Spinner
  , colored : Bool
  }


init : (Model, Cmd Msg)
init =
  ( { spinner1 = SpinnerState (Tuple.first Spinner.init) True
    , spinner2 = SpinnerState (Tuple.first Spinner.init) False
    }
  , Cmd.batch
      [ Cmd.map (\msg -> SpinnerMsg msg 1) <| Tuple.second Spinner.init
      , Cmd.map (\msg -> SpinnerMsg msg 2) <| Tuple.second Spinner.init
      ]
  )


type Msg
  = SpinnerMsg Spinner.Msg Int
  | ToggleColor Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SpinnerMsg msg 1 ->
      let
        (spinner, cmd) =
          Spinner.update msg model.spinner1.spinner

        spinner1 = model.spinner1

        newSpinner1 =
          { spinner1 | spinner = spinner }
      in
        { model | spinner1 = newSpinner1 } ! [ Cmd.map (\msg -> SpinnerMsg msg 1) cmd ]

    SpinnerMsg msg 2 ->
      let
        (spinner, cmd) =
          Spinner.update msg model.spinner2.spinner

        spinner2 = model.spinner2

        newSpinner2 =
          { spinner2 | spinner = spinner }
      in
        { model | spinner2 = newSpinner2 } ! [ Cmd.map (\msg -> SpinnerMsg msg 2) cmd ]

    ToggleColor 1 ->
      let
        spinner1 = model.spinner1

        newSpinner1 =
          { spinner1 | colored = not spinner1.colored }
      in
        { model | spinner1 = newSpinner1 } ! []

    ToggleColor 2 ->
      let
        spinner2 = model.spinner2

        newSpinner2 =
          { spinner2 | colored = not spinner2.colored }
      in
        { model | spinner2 = newSpinner2 } ! []

    _ ->
      model ! []


view : Model -> Html Msg
view model =
  div []
    [ AltHtml.map (\msg -> SpinnerMsg msg 1) <| Spinner.view 100 model.spinner1.colored model.spinner1.spinner
    , AltHtml.map (\msg -> SpinnerMsg msg 2) <| Spinner.view 200 model.spinner2.colored model.spinner2.spinner
    ]
