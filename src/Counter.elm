port module Counter exposing (..)


import AltHtml exposing (..)


main : Program Never Model Msg
main = beginnerProgram
  { model = 0
  , update = update
  , view = view
  }


type alias Model = Int


type Msg = Increment | Decrement


update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (toString model) ]
    , button [ onClick Increment ] [ text "+" ]
    ]
