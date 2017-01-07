import AltHtml exposing (..)

import Task

main =
  beginnerProgram
    { model = model, view = view, update = update }


model = Nothing

model_ =
  { a = 0
  , b = [0,1,2,3,4,5,6,7,8,9,1,2,3,4,5,6,7,8,9]
  , c = [0,1,2,3,4,5,6,7,8,9,1,2,3,4,5,6,7,8,9]
  -- , d = Task.succeed ()
  , e = [0,1,2,3,4,5,6,7,8,9,1,2,3,4,5,6,7,8,9]
  , f = [0,1,2,3,4,5,6,7,8,9,1,2,3,4,5,6,7,8,9]
  , g = [0,1,2,3,4,5,6,7,8,9,1,2,3,4,5,6,7,8,9]
  , h = {a=1,b=1,c=1,d=1,e="1",f="1",g="1",h="1",i="1",j="1",k="1"}
  , i = 0
  , j = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  , k = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
  , l = "ccccccccccccccccccccccccccccccccccc"
  , o = "dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"
  , p = Just (Just (Just (Just (Just (Just (Just (Just (Just 1))))))))
  , q = Just (Just (Just (Just (Just (Just (Just (Just (Just 2))))))))
  , r = Just (Just (Just (Just (Just (Just (Just (Just (Just 3))))))))
  , s = Just (Just (Just (Just (Just (Just (Just (Just (Just 4))))))))
  , t = "Ok, Google"
  , u = 123456789
  , v = 123.456
  , w = [0,1,2,3,4,5,6,7,8,9,1,2,3,4,5,6,7,8,9]
  , x = [0,1,2,3,4,5,6,7,8,9,1,2,3,4,5,6,7,8,9]
  , y = [0,1,2,3,4,5,6,7,8,9,1,2,3,4,5,6,7,8,9]
  , z = div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text "" ]
    , button [ onClick Increment ] [ text "+" ]
    ]
  }


view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (toString model) ]
    , button [ onClick Increment ] [ text "+" ]
    ]


type Msg = Increment | Decrement


update msg model =
  case msg of
    Increment ->
      Just model_

    Decrement ->
      Nothing
