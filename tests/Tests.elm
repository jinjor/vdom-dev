module Tests exposing (..)

import Test exposing (..)
import Expect
import VirtualDom.History as History exposing (Address)


empty = History.empty 0


update msg model = (model + msg, ())


historyA =
 [ History.add (Just 0) 1
 , History.add (Just 0) 2
 , History.add (Just 0) 3
 ]
 |> List.foldl (\f history -> f 0 history) empty


type Msg = A | B | C Int | D Msg | E Int Msg


updateB msg model = (model + 1, ())


historyB =
  [ History.add Nothing A           -- 0
  , History.add Nothing A
  , History.add Nothing (E 1 A)     -- 1
  , History.add Nothing (E 2 A)
  , History.add Nothing (E 2 B)     -- 2
  , History.add Nothing (E 2 (C 1)) -- 3
  , History.add Nothing (E 2 (C 2))
  , History.add Nothing (E 2 (D A)) -- 4
  , History.add Nothing (E 1 (D A))
  ]
  |> List.foldl (\f history -> f 0 history) empty


historyB2 =
  (  List.repeat 1000 ( History.add Nothing A 0 )
  ++ List.repeat 1 ( History.add Nothing B 1000 )
  )
  |> List.foldl (\f history -> f history) empty


historyC =
  (  List.repeat 10 ( History.add Nothing A) -- 0 0 0-9 closed
  ++ List.repeat 10 ( History.add Nothing B) -- 0 1 0-9 closed
  ++ List.repeat 10 ( History.add (Just 1) A) -- 0 2 0-9 closed
  ++ List.repeat 10 ( History.add (Just 1) B) -- 0 3 0-9 closed
  )
  |> List.foldl (\f history -> f 0 history) empty


historyC2 =
  [ History.add Nothing A -- 0 0 0 (open)
  , History.add Nothing B -- 0 1 0
  , History.add Nothing A -- 0 2 0
  ]
  |> List.foldl (\f history -> f 0 history) empty


historyC3 =
  [ History.add (Just 1) A    -- 0 0 0 (open)
  , History.add (Just 1000) A -- 0 0 1
  , History.add (Just 2000) A -- 0 0 2
  ]
  |> List.foldl (\f history -> f 0 history) empty


assertGet history expect snapshotIndex groupIndex msgIndex = \() ->
  Expect.equal expect <|
    History.get
      update
      (Address snapshotIndex groupIndex msgIndex)
      history


assertAdd history expect snapshotIndex groupIndex msgIndex = \() ->
  Expect.equal expect <|
  Tuple.first <|
    History.get
      updateB
      (Address snapshotIndex groupIndex msgIndex)
      history


assertShift history expect forward snapshotIndex groupIndex msgIndex = \() ->
  Expect.equal expect <|
    (if forward then History.visibleAddressAfter else History.visibleAddressBefore)
      (Address snapshotIndex groupIndex msgIndex)
      history


assertLatest history expect = \() ->
  Expect.equal expect (History.latestAddress history)


testHistory =
  describe "History"
    [ test "get" <| assertGet historyA (1, 1) 0 0 0
    , test "get" <| assertGet historyA (3, 2) 0 1 0
    , test "get" <| assertGet historyA (6, 3) 0 2 0
    , test "add" <| assertAdd historyB (1) 0 0 0
    , test "add" <| assertAdd historyB (2) 0 0 1
    , test "add" <| assertAdd historyB (3) 0 1 0
    , test "add" <| assertAdd historyB (4) 0 1 1
    , test "add" <| assertAdd historyB (5) 0 2 0
    , test "add" <| assertAdd historyB (6) 0 3 0
    , test "add" <| assertAdd historyB (7) 0 3 1
    , test "add" <| assertAdd historyB (8) 0 4 0
    , test "add" <| assertAdd historyB (9) 0 4 1
    , test "add across snapshot" <| assertAdd historyB2 (1000) 0 0 999
    , test "add across snapshot" <| assertAdd historyB2 (1001) 1 0 0
    , test "jump up 1" <| assertShift historyC (Just (Address 0 0 0)) False 0 0 5
    , test "jump up 1" <| assertShift historyC (Just (Address 0 0 9)) False 0 1 5
    , test "jump up 1" <| assertShift historyC (Just (Address 0 1 9)) False 0 2 5
    , test "jump up 1" <| assertShift historyC (Just (Address 0 2 9)) False 0 3 5
    , test "jump down 1" <| assertShift historyC (Just (Address 0 1 0)) True 0 0 5
    , test "jump down 1" <| assertShift historyC (Just (Address 0 2 0)) True 0 1 5
    , test "jump down 1" <| assertShift historyC (Just (Address 0 3 0)) True 0 2 5
    , test "jump down 1" <| assertShift historyC (Nothing) True 0 3 5
    , test "jump up 2" <| assertShift historyC2 (Just (Address 0 0 0)) False 0 0 0
    , test "jump up 2" <| assertShift historyC2 (Just (Address 0 0 0)) False 0 1 0
    , test "jump up 2" <| assertShift historyC2 (Just (Address 0 1 0)) False 0 2 0
    , test "jump down 3" <| assertShift historyC2 (Just (Address 0 1 0)) True 0 0 0
    , test "jump down 3" <| assertShift historyC2 (Just (Address 0 2 0)) True 0 1 0
    , test "jump down 3" <| assertShift historyC2 (Nothing) True 0 2 0
    , test "jump up 4" <| assertShift historyC3 (Just (Address 0 0 0)) False 0 0 0
    , test "jump up 4" <| assertShift historyC3 (Just (Address 0 0 0)) False 0 0 1
    , test "jump up 4" <| assertShift historyC3 (Just (Address 0 0 1)) False 0 0 2
    , test "jump down 4" <| assertShift historyC3 (Just (Address 0 0 1)) True 0 0 0
    , test "jump down 4" <| assertShift historyC3 (Just (Address 0 0 2)) True 0 0 1
    , test "jump down 4" <| assertShift historyC3 (Nothing) True 0 0 2
    , test "latestAddress" <| assertLatest empty Nothing
    , test "latestAddress" <| assertLatest historyA (Just <| Address 0 2 0)
    , test "latestAddress" <| assertLatest historyB (Just <| Address 0 4 1)
    , test "latestAddress" <| assertLatest historyB2 (Just <| Address 1 0 0)
    , test "latestAddress" <| assertLatest historyC (Just <| Address 0 3 9)
    , test "latestAddress" <| assertLatest historyC2 (Just <| Address 0 2 0)
    , test "latestAddress" <| assertLatest historyC3 (Just <| Address 0 0 2)
    ]


all : Test
all =
  describe "Debugger"
    [ testHistory
    ]
