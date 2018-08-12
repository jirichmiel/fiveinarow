module AppModelGame exposing (..)

import Matrix exposing (..)

import App.Model.Game exposing (..)

import Expect exposing (Expectation)
import Dict exposing (..)
--import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

-- filter for parsing logs cat /home/jiri/Downloads/test.txt | grep -Eo '(FORWARD|LEAF|WINNER|move = [^,]+,[^,]+,[^,]+|value = [^,]+|alpha = [^,]+|beta = [^,]+)'

suite : Test
suite =
  describe "App/Model/Game.elm"
  [
    test "Function fivesInALine must generate a five correctly." <|
      \() ->
        Expect.equal (App.Model.Game.fivesInALine (3,2) (1,-1)) [((-1,6),(1,-1)),((0,5),(1,-1)),((1,4),(1,-1)),((2,3),(1,-1)),((3,2),(1,-1))]

  , test "Function affectedFives must generata all affected fives correctly." <|
      \() ->
        Expect.equal (App.Model.Game.affectedFives (3,3)) 
          [ (( 3,-1),(0, 1)),((3,0),(0, 1)),((3,1),(0, 1)),((3,2),(0, 1)),((3,3),(0, 1))
          , ((-1, 3),(1, 0)),((0,3),(1, 0)),((1,3),(1, 0)),((2,3),(1, 0)),((3,3),(1, 0))
          , ((-1,-1),(1, 1)),((0,0),(1, 1)),((1,1),(1, 1)),((2,2),(1, 1)),((3,3),(1, 1))
          , ((-1, 7),(1,-1)),((0,6),(1,-1)),((1,5),(1,-1)),((2,4),(1,-1)),((3,3),(1,-1)) 
          ]

  --, test "Function updateCounter for black's stone." <|
  --    \() ->
  --      Expect.equal (App.Model.Game.updateCounter Black (4,6)) (5,6)

  --, test "Function updateCounter for white's stone." <|
  --    \() ->
  --      Expect.equal (App.Model.Game.updateCounter White (4,6)) (4,7)     

  , test "Function updateCounterFunc for a counter and black's stone." <|   
      \() ->
        Expect.equal (App.Model.Game.updateCounterFunc Black (Just (4,6))) (Just (5,6))

  , test "Function updateCounterFunc for Nothing." <|
      \() ->
        Expect.equal (App.Model.Game.updateCounterFunc Black Nothing) Nothing    

  , test "Function updateCounters for empty fives." <|
      \() ->
        Expect.equal (App.Model.Game.updateCounters (Dict.fromList [(((3,3),(0,1)), (4,6))]) [] (App.Model.Game.updateCounterFunc Black)) (Dict.fromList [(((3,3),(0,1)), (4,6))])

  , test "Function updateCounters for not existed five." <|
      \() ->
        Expect.equal (App.Model.Game.updateCounters (Dict.fromList [(((3,3),(0,1)), (4,6))]) [((4,3),(0,1))] (App.Model.Game.updateCounterFunc Black)) (Dict.fromList [(((3,3),(0,1)), (4,6))])

  , test "Function updateCounters for correct five." <|
      \() ->
        Expect.equal (App.Model.Game.updateCounters (Dict.fromList [(((3,3),(0,1)), (4,6))]) [((3,3),(0,1))] (App.Model.Game.updateCounterFunc Black)) (Dict.fromList [(((3,3),(0,1)), (5,6))])

  , test "Function sumDeltaValueCounters." <|
      \() ->
        Expect.equal (App.Model.Game.sumDeltaValueCounters Black (Dict.fromList [(((3,3),(0,1)), (3,0))]) [((3,3),(0,1))]) ((0,0,-1,1,0),(0,0,0,0,0))

  , test "Function sumDeltaValueCounters not existed five." <|
      \() ->
        Expect.equal (App.Model.Game.sumDeltaValueCounters Black (Dict.fromList [(((3,3),(0,1)), (3,0))]) [((-1,3),(0,1))]) ((0,0,0,0,0),(0,0,0,0,0))

  , test "Test generate matrix." <|
      \() ->
        Expect.equal (List.range 0 5 |> List.map (\x -> List.map2 (,) (List.repeat 6 x) (List.range 0 5) ) |> List.concat) (
          [
            (0,0),(0,1),(0,2),(0,3),(0,4),(0,5),
            (1,0),(1,1),(1,2),(1,3),(1,4),(1,5),
            (2,0),(2,1),(2,2),(2,3),(2,4),(2,5),
            (3,0),(3,1),(3,2),(3,3),(3,4),(3,5),
            (4,0),(4,1),(4,2),(4,3),(4,4),(4,5),
            (5,0),(5,1),(5,2),(5,3),(5,4),(5,5)
          ]
        )

  --, test "Function isInRange_ 5 (0,1),(0,1)." <|
  --    \() ->
  --      Expect.equal (App.Model.Game.isInRange_ 5 (0,1) (0,1)) True

  --, test "Function createEmptyCountersForDirection_ 5 1,1" <|
  --    \() ->
  --      Expect.equal (App.Model.Game.createEmptyCountersForDirection_ 5 (0,1)) (
  --        [
  --          (((0,0),(0,1)),(0,0)), (((0,1),(0,1)),(0,0)), (((1,0),(0,1)),(0,0)), (((1,1),(0,1)),(0,0)), (((2,0),(0,1)),(0,0)), (((2,1),(0,1)),(0,0)),
  --          (((3,0),(0,1)),(0,0)), (((3,1),(0,1)),(0,0)), (((4,0),(0,1)),(0,0)), (((4,1),(0,1)),(0,0)), (((5,0),(0,1)),(0,0)), (((5,1),(0,1)),(0,0))
  --        ]
  --      )

  , test "Function createEmptyCounters." <|
      \() ->
        Expect.equal (App.Model.Game.createEmptyCounters 5) (Dict.fromList
          [
            (((0,0),(0,1)),(0,0)), (((0,0),(1,0)),(0,0)), (((0,0),(1,1)),(0,0)),
            (((0,1),(0,1)),(0,0)), (((0,1),(1,0)),(0,0)), (((0,1),(1,1)),(0,0)),
            (((0,2),(1,0)),(0,0)),
            (((0,3),(1,0)),(0,0)),
            (((0,4),(1,0)),(0,0)), (((0,4),(1,-1)),(0,0)),
            (((0,5),(1,0)),(0,0)), (((0,5),(1,-1)),(0,0)),

            (((1,0),(0,1)),(0,0)), (((1,0),(1,0)),(0,0)), (((1,0),(1,1)),(0,0)),
            (((1,1),(0,1)),(0,0)), (((1,1),(1,0)),(0,0)), (((1,1),(1,1)),(0,0)),
            (((1,2),(1,0)),(0,0)),
            (((1,3),(1,0)),(0,0)),
            (((1,4),(1,0)),(0,0)), (((1,4),(1,-1)),(0,0)),
            (((1,5),(1,0)),(0,0)), (((1,5),(1,-1)),(0,0)),

            (((2,0),(0,1)),(0,0)), (((2,1),(0,1)),(0,0)),
            (((3,0),(0,1)),(0,0)), (((3,1),(0,1)),(0,0)),
            (((4,0),(0,1)),(0,0)), (((4,1),(0,1)),(0,0)),
            (((5,0),(0,1)),(0,0)), (((5,1),(0,1)),(0,0))

          ])

    , test "Function locationsForFive." <|
      \() ->
        Expect.equal (App.Model.Game.locationsForFive ((12,13),(1,-1))) [ (12,13), (13,12), (14,11), (15,10), (16,9) ]

    , test "Function createGame creates a matrix with correct number of rows." <|
      \() ->
        Expect.equal (testCreateGame 10 |> App.Model.Game.board |> Matrix.rowCount) 10

    , test "Function createGame creates a matrix with correct number of cols." <|
      \() ->
        Expect.equal (testCreateGame 10 |> App.Model.Game.board |> Matrix.colCount) 10

    , test "Function createGame creates a matrix with all empty fields." <|
      \() ->
        Expect.equal (testCreateGame 10 |> App.Model.Game.board |> Matrix.flatten |> List.filter (\x -> x /= Empty) |> List.length) 0

    , test "Function createGame creates a matrix with zero vector values." <|
      \() ->
        Expect.equal (testCreateGame 10 |> App.Model.Game.value) ((0,0,0,0,0),(0,0,0,0,0))

    , test "Function createGame creates a matrix with one possible move." <|
      \() ->
        Expect.equal (testCreateGame 10 |> App.Model.Game.moves) [({player = Black, location = (5,5)})]

    , test "Function createGame creates a matrix with correct numbers of counters." <|
      \() ->
        Expect.equal (testCreateGame 10 |> App.Model.Game.counters |> Dict.size) 192

    , test "After first move there must be only one non-empty field." <|
      \() ->
        Expect.equal (testGame1FirstMove |> App.Model.Game.board |> Matrix.flatten |> List.filter (\x -> x /= Empty) |> List.length) 1

    , test "After first move field 5,5 must be Black." <|
      \() ->
        Expect.equal (testGame1FirstMove |> App.Model.Game.board |> Matrix.get (5,5)) (Just (Stone Black))

    , test "After first move there must be 8 possible moves for White." <|
      \() ->
        Expect.equal (testGame1FirstMove |> App.Model.Game.moves) 
          [ {player = White, location = (4,4)}
          , {player = White, location = (5,4)}
          , {player = White, location = (6,4)}
          , {player = White, location = (4,5)}
          , {player = White, location = (6,5)}
          , {player = White, location = (4,6)}
          , {player = White, location = (5,6)}
          , {player = White, location = (6,6)}
          ]

    --, test "Possible moves function for a few moves." <|
    --    \() ->
    --      Expect.equal (App.Model.Game.possibleMovesFunc testGameMatrix3)
    --      [ {player = White, location = (2,2)}
    --      , {player = White, location = (3,2)}
    --      , {player = White, location = (4,2)}
    --      , {player = White, location = (2,3)}
    --      , {player = White, location = (2,4)}
    --      , {player = White, location = (2,5)}
    --      , {player = White, location = (3,5)}
    --      , {player = White, location = (4,5)}
    --      , {player = White, location = (5,4)}
    --      , {player = White, location = (6,4)}
    --      , {player = White, location = (6,5)}
    --      , {player = White, location = (4,6)}
    --      , {player = White, location = (5,6)}
    --      , {player = White, location = (6,6)}
    --      , {player = White, location = (5,2)}
    --      , {player = White, location = (5,3)}
    --      ]

    , test "After first move there must be 20 affected counters around 5,5." <|
      \() ->
        Expect.equal (testGame1FirstMove |> App.Model.Game.counters |> Dict.foldl (
          \(loc,dic) counter bool -> 
            case bool of
              False -> False
              True -> 
                if (
                  (dic == (0,1) && (List.member loc [(5,5),(5,4),(5,3),(5,2),(5,1)]))
                  || (dic == (1,0) && (List.member loc [(5,5),(4,5),(3,5),(2,5),(1,5)]))
                  || (dic == (1,1) && (List.member loc [(5,5),(4,4),(3,3),(2,2),(1,1)]))
                  || (dic == (1,-1) && (List.member loc [(5,5),(4,6),(3,7),(2,8),(1,9)]))
                ) then
                  counter == (1,0) -- there is count up black stone on affected fives
                else
                  counter == (0,0) -- nothing is count up on other fives
        ) True) True 

    , test "Test that there is no stone counter for five ((0,-1),(1,1))" <|
      \() ->
        Expect.equal (Dict.get ((0,-1),(1,1)) (testGame1FirstMove |> App.Model.Game.counters)) Nothing

    , test "Test that there is properly counter for five ((4,4),(1,1))" <|
      \() ->
        Expect.equal (Dict.get ((4,4),(1,1)) (testGame1FirstMove |> App.Model.Game.counters)) (Just (1,0))

    , test "Test sumDeltaValueCounters after second move." <|
      \() ->
        Expect.equal (sumDeltaValueCounters White (testGame1FirstMove |> App.Model.Game.counters) (App.Model.Game.affectedFives (4,4))) ((-4,0,0,0,0),(16,0,0,0,0))

    , test "After first move game value must be 20 1xstone fives for black and nothing for white." <|
      \() ->
        Expect.equal (testGame1FirstMove |> App.Model.Game.value) ((20,0,0,0,0),(0,0,0,0,0))

    , test "After second move game value must be 16 1xstone fives for black and 16 1xstone for white." <|
      \() ->
        Expect.equal (testGame1SecondMove |> App.Model.Game.value) ((16,0,0,0,0),(16,0,0,0,0))

    , test "After first move game no. 2 value must be 20 1xstone fives for black and nothing for white." <|
      \() ->
        Expect.equal (testGame2FirstMove |> App.Model.Game.value) ((20,0,0,0,0),(0,0,0,0,0))

    , test "After second move game no. 2 value must be 16 1xstone fives for black and 13 1xstones for white." <|
      \() ->
        Expect.equal (testGame2SecondMove |> App.Model.Game.value) ((16,0,0,0,0),(13,0,0,0,0))

    , test "After third move game no. 2 value must be 25 1xstone and 4 2xstones fives for black and 13 1xstones for white." <|
      \() ->
        Expect.equal (testGame2ThirdMove |> App.Model.Game.value) ((23,4,0,0,0),(13,0,0,0,0))

    --, test "After third move game no. 2, total game value for black must be 11." <|
    --  \() ->
    --    Expect.equal (App.Model.Game.valueFunc App.Model.Game.basic Black testGame2ThirdMove (Just { player = Black, location = (0,0)})) 9 -- location is not important

    --, test "After third move game no. 2, total game value for white must be -117." <|
    --  \() ->
    --    Expect.equal (App.Model.Game.valueFunc App.Model.Game.basic Black testGame2ThirdMove (Just { player = White, location = (0,0)})) -113 -- location is not important

    --, test "Where move CPU after third move game no. 2." <|
    --  \() ->
    --    Expect.equal (App.Model.Game.cpuMove App.Model.Game.expert testGame2ThirdMove) (Just (3,3))

    , test "Test Game Matrix 1" <|
      \() ->
        Expect.equal (App.Model.Game.cpuMove App.Model.Game.expert testGameMatrix1) (Just (0,8))

    , test "Test Game Matrix 2" <|
      \() ->
        Expect.equal (App.Model.Game.cpuMove App.Model.Game.expert testGameMatrix2) (Just (1,3))

    , test "Test Game Matrix 2b" <|
      \() ->
        Expect.equal (App.Model.Game.cpuMove App.Model.Game.expert testGameMatrix2) (Just (1,3))

    , test "Test Game Matrix 3" <|
      \() ->
        Expect.equal (App.Model.Game.cpuMove App.Model.Game.expert testGameMatrix3) (Just (2,2))

  ]


testCreateGame : Int -> GameBoardEx
testCreateGame n =
  App.Model.Game.createGame n

testGame1FirstMove : GameBoardEx
testGame1FirstMove = 
  testCreateGame 10 |> testMove {player = Black, location = (5,5) }

testGame1SecondMove : GameBoardEx
testGame1SecondMove = 
  testGame1FirstMove |> testMove {player = White, location = (4,4) }


testGame2FirstMove : GameBoardEx
testGame2FirstMove = 
  testCreateGame 9 |> testMove {player = Black, location = (4,4) }

testGame2SecondMove : GameBoardEx
testGame2SecondMove = 
  testGame2FirstMove |> testMove {player = White, location = (3,4) }

testGame2ThirdMove : GameBoardEx
testGame2ThirdMove = 
  testGame2SecondMove |> testMove {player = Black, location = (5,5) }

{- MATRIX 1 -----------------------------------}

{-
    0 1 2 3 4 5 6 7 8
0   _ _ _ _ _ _ _ _ _
1   _ _ _ _ _ _ _ x _
2   _ _ _ _ _ _ x _ _
3   _ _ o o x x x x o
4   _ _ _ o x o _ _ _
5   _ _ _ o o _ _ _ _
6   _ _ _ _ _ x _ _ _
7   _ _ _ _ _ _ _ _ _
8   _ _ _ _ _ _ _ _ _
-}

testGameMatrix1 : GameBoardEx
testGameMatrix1 = 
  App.Model.Game.createGame 9
    |> testMove { player = Black, location = (1,7) }
    |> testMove { player = White, location = (3,2) }
    |> testMove { player = Black, location = (2,6) }
    |> testMove { player = White, location = (3,3) }
    |> testMove { player = Black, location = (3,4) }
    |> testMove { player = White, location = (3,8) }
    |> testMove { player = Black, location = (3,5) }
    |> testMove { player = White, location = (4,3) }
    |> testMove { player = Black, location = (3,6) }
    |> testMove { player = White, location = (4,5) }
    |> testMove { player = Black, location = (3,7) }
    |> testMove { player = White, location = (5,3) }
    |> testMove { player = Black, location = (4,4) }
    |> testMove { player = White, location = (5,4) }
    |> testMove { player = Black, location = (6,5) }


{- MATRIX 2 -----------------------------------}

{-
    0 1 2 3 4 5 6 7 8
0   _ _ _ _ _ _ _ _ _
1   _ _ _ _ _ _ _ _ _
2   _ _ _ o _ x _ _ _
3   _ _ _ o o o x _ _
4   _ _ _ o x _ x _ _
5   _ _ o x _ x _ _ _
6   _ o _ _ x _ _ _ _
7   x _ _ _ _ _ _ _ _
8   _ _ _ _ _ _ _ _ _
-}
testGameMatrix2 : GameBoardEx
testGameMatrix2 = 
  App.Model.Game.createGame 9
    |> testMove { player = Black, location = (2,5) }
    |> testMove { player = White, location = (2,3) }
    |> testMove { player = Black, location = (3,6) }
    |> testMove { player = White, location = (3,3) }
    |> testMove { player = Black, location = (4,4) }
    |> testMove { player = White, location = (3,4) }
    |> testMove { player = Black, location = (4,6) }
    |> testMove { player = White, location = (3,5) }
    |> testMove { player = Black, location = (5,3) }
    |> testMove { player = White, location = (4,3) }
    |> testMove { player = Black, location = (5,6) }
    |> testMove { player = White, location = (5,2) }
    |> testMove { player = Black, location = (6,4) }
    |> testMove { player = White, location = (6,1) }
    |> testMove { player = Black, location = (7,0) }

testGameMatrix2b : GameBoardEx
testGameMatrix2b = 
  App.Model.Game.createGame 9
    |> testMove { player = Black, location = (4,4) }
    |> testMove { player = White, location = (4,3) }
    |> testMove { player = Black, location = (5,5) }
    |> testMove { player = White, location = (3,3) }
    |> testMove { player = Black, location = (5,3) }
    |> testMove { player = White, location = (5,2) }
    |> testMove { player = Black, location = (6,4) }
    |> testMove { player = White, location = (6,1) }
    |> testMove { player = Black, location = (7,0) }
    |> testMove { player = White, location = (3,4) }
    |> testMove { player = Black, location = (2,5) }
    |> testMove { player = White, location = (3,5) }
    |> testMove { player = Black, location = (3,6) }
    |> testMove { player = White, location = (2,3) }
    |> testMove { player = Black, location = (4,6) }

{- MATRIX 3 -----------------------------------}

{-
    0 1 2 3 4 5 6 7 8
0   _ _ _ _ _ _ _ _ _
1   _ _ _ _ _ _ _ _ _
2   _ _ _ _ _ _ _ _ _
3   _ _ _ x o _ _ _ _
4   _ _ _ o x _ _ _ _
5   _ _ _ _ _ x _ _ _
6   _ _ _ _ _ _ _ _ _
7   _ _ _ _ _ _ _ _ _
8   _ _ _ _ _ _ _ _ _
-}

testGameMatrix3 : GameBoardEx
testGameMatrix3 = 
  App.Model.Game.createGame 9
    |> testMove { player = Black, location = (4,4) }
    |> testMove { player = White, location = (4,3) }
    |> testMove { player = Black, location = (5,5) }
    |> testMove { player = White, location = (3,4) }
    |> testMove { player = Black, location = (3,3) }
    
  
testMove : GameMove -> GameBoardEx -> GameBoardEx
testMove move boardEx =
  App.Model.Game.moveFuncEx boardEx move.location
