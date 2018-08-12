module App.Model.Game exposing (..)

import Minimax exposing (minimax, Node)
import Dict exposing (..)
import Set exposing (..)
import Matrix exposing (Matrix, Location, get, set, loc, rowCount, colCount, flatten)

{-| Represent game state.

    GameInProgress - the game is still running.
    
    Draw - the game is over, the game board is full without that 
    one of the players wins.
    
    Winner Color (List Location) - one of the players wins, Color determines 
    the winner, (List Location) points to the winner's five stones 
-}
type GameState 
  = GameInProgress | Draw | Winner Color (List Location)

{-| Represent one of players.
-}
type Color
  = Black | White

{-| Represent the type of game field.
-}
type Field
  = Empty | Stone Color

{-| Represent game board - a matrix of the field.
-}
type alias GameBoard
  = Matrix Field

{-| Represent game move.
-}
type alias GameMove =
  { player : Color
  , location : Location 
  }

{-| Number of black's stones and white's stones
-}
type alias Counter
  = (Int, Int)

{-| Five points to stone counter
-}
type alias BoardCounter = Dict Five Counter

type alias GameBoardEx =
  { board    : GameBoard          -- standard game board (for rendering)
  , counters : BoardCounter       -- counters for quickly computing of the game value
  , moves    : List GameMove      -- list of possible moves
  , values   : (Vector5, Vector5) -- value vector for black and value vector for white
  , player   : Color              -- who is on move
  }

-------------------------------------------------------------------------------
-- CREATE GAME FUNCTIONS
-------------------------------------------------------------------------------

{-|
-}
createEmptyGame : GameBoardEx
createEmptyGame =
  createGame 0

{-|
-}
createGame : Int -> GameBoardEx
createGame size =
  { board    = Matrix.square size (\location -> Empty)
  , counters = createEmptyCounters (size-1)
  , moves    = [{ player = Black, location = (size // 2, size // 2) }]
  , values   = ((0,0,0,0,0),(0,0,0,0,0))
  , player   = Black
  }

createEmptyCounters : Int -> Dict Five Counter
createEmptyCounters limit =
  allDirections
    |> List.map (\dir -> createEmptyCountersForDirection limit dir)
    |> List.concat
    |> Dict.fromList

createEmptyCountersForDirection : Int -> Direction -> List (Five, Counter)
createEmptyCountersForDirection limit direction =
  List.range 0 limit
    |> List.map (\x -> List.map2 (,) (List.repeat (limit+1) x) (List.range 0 limit) )
    |> List.concat
    |> List.filter (isInRange limit direction)
    |> List.map (createCounterItem direction)

isInRange : Int -> Direction -> Location -> Bool
isInRange limit (dx, dy) (x, y) =
  let
    cx = x+5*dx
    cy = y+5*dy
  in 
    (-1 <= cx) && (cx <= limit+1) && (-1 <= cy) && (cy <= limit+1)

createCounterItem : Direction -> Location -> (Five, Counter)
createCounterItem dir loc =
  ((loc, dir),(0,0))

-------------------------------------------------------------------------------
-- CPU MOVE FUNCTIONS
-------------------------------------------------------------------------------

{-|
-}
cpuMove : AlgParams -> GameBoardEx -> Maybe Location
cpuMove algParams boardEx =
  let
    node = minimax move (gameValue algParams boardEx.player) possibleMoves boardEx algParams.depth
  in
    case (node.move) of
      Nothing -> Nothing
      Just move -> Just move.location

gameValue : AlgParams -> Color -> Node GameBoardEx GameMove -> Int
gameValue algParams player node =
  let
    boardEx = node.position
    lastMove = node.move
    blacks = Tuple.first boardEx.values
    whites = Tuple.second boardEx.values
  in
  case lastMove of
    Nothing -> 0
    Just x -> 
      case player of 
        Black -> (scalar algParams.attackRatings blacks) - (scalar algParams.defenseRatings whites)
        White -> (scalar algParams.attackRatings whites) - (scalar algParams.defenseRatings blacks)

possibleMoves : Node GameBoardEx GameMove -> List GameMove
possibleMoves node =
  let
    boardEx = node.position
  in
  boardEx.moves

moveFuncEx : GameBoardEx -> Location -> GameBoardEx
moveFuncEx boardEx location =
  let
    gameMove = {player = boardEx.player, location = location}
    fives = affectedFives gameMove.location
    counters = updateCounters boardEx.counters fives (updateCounterFunc gameMove.player)
    canPlay = canProgress counters
  in 
    { boardEx 
    | board    = Matrix.set gameMove.location (Stone gameMove.player) boardEx.board
    , counters = counters
    , values   = superSum boardEx.values (sumDeltaValueCounters gameMove.player boardEx.counters fives)
    , moves    = if (canPlay) then 
        boardEx.moves 
          |> List.map (\n -> n.location) 
          |> updatePossibleLocations boardEx.board gameMove.location
          |> List.map (\n -> {player = oppositeColor gameMove.player, location = n} )
      else 
        []
    , player = oppositeColor boardEx.player
    
    }

move : Node GameBoardEx GameMove -> GameMove -> GameBoardEx
move node gameMove =
  moveFuncEx node.position gameMove.location


updatePossibleLocations : GameBoard -> Location -> List Location -> List Location
updatePossibleLocations board location possibleLocations =
  possibleLocations
    |> List.append (neighbors location) -- append neiggbors of current location
    |> unique -- no location must be in the list twice
    |> List.filter (\(r,c) -> (r >= 0) && (c >= 0) && (r < Matrix.rowCount board) && (c < Matrix.colCount board)) -- location must be within board
    |> List.filter (\l -> get l board == Empty)  -- we cannot put two stones on the one location - so location must be empty
    |> List.filter (\l -> l /= location)  -- filter also current move

sumDeltaValueCounters : Color -> BoardCounter -> List Five -> (Vector5, Vector5)
sumDeltaValueCounters color counters fives =
  case fives of
    [] -> ((0,0,0,0,0),(0,0,0,0,0))
    (headFive :: remainFives) -> superSum (deltaValueFunc color (Dict.get headFive counters)) (sumDeltaValueCounters color counters remainFives)

deltaValueFunc : Color -> Maybe (Int, Int) -> (Vector5, Vector5)
deltaValueFunc color x =
  case x of
    Nothing -> ((0,0,0,0,0),(0,0,0,0,0))
    Just c -> deltaValue color c

deltaValue : Color -> Counter -> (Vector5, Vector5)
deltaValue color counter =
  swapOnWhite color <|
    case (swapOnWhite color counter) of
      (0, 0) -> ((1,0,0,0,0),(0,0,0,0,0))  -- empty five -> attack to 1xstone
      (1, 0) -> ((-1,1,0,0,0),(0,0,0,0,0)) -- attack -> move from 1xstone to 2xstone
      (2, 0) -> ((0,-1,1,0,0),(0,0,0,0,0)) -- --||--
      (3, 0) -> ((0,0,-1,1,0),(0,0,0,0,0)) -- --||--
      (4, 0) -> ((0,0,0,-1,1),(0,0,0,0,0)) -- --||--
      (0, 1) -> ((0,0,0,0,0),(-1,0,0,0,0)) -- block opponent's 1xstone
      (0, 2) -> ((0,0,0,0,0),(0,-1,0,0,0)) -- block opponent's 2xstone
      (0, 3) -> ((0,0,0,0,0),(0,0,-1,0,0)) -- --||--
      (0, 4) -> ((0,0,0,0,0),(0,0,0,-1,0)) -- --||--
      _      -> ((0,0,0,0,0),(0,0,0,0,0))  -- five is already blocked

swapOnWhite : Color -> (a, a) -> (a, a)
swapOnWhite color (x, y) =
  case color of
    Black -> (x, y)
    White -> (y, x)

oppositeColor : Color -> Color
oppositeColor color =
  case color of
    Black -> White
    White -> Black

updateCounters : BoardCounter -> List Five -> (Maybe Counter -> Maybe Counter) -> BoardCounter
updateCounters counters fives countFunction =
  case fives of
    [] -> counters
    (head :: remains) -> updateCounters (Dict.update head countFunction counters) remains countFunction

updateCounterFunc : Color -> Maybe Counter -> Maybe Counter
updateCounterFunc color counter =
  case counter of
    Nothing -> Nothing
    Just (black, white) -> 
      case color of
        Black -> Just (black+1, white)
        White -> Just (black, white+1)

-------------------------------------------------------------------------------
-- STATE FUNCTIONS
-------------------------------------------------------------------------------

canProgress : BoardCounter -> Bool
canProgress counters =
  Dict.toList counters 
    |> List.filter (\(f, (b,w)) -> (b>=5 || w>=5))
    |> List.isEmpty

isGameOver : GameBoardEx -> Bool
isGameOver board =
  gameState board /= GameInProgress

isWinnerLocation : GameBoardEx -> Location -> Bool
isWinnerLocation board location =
  case gameState board of
    Winner _ locations -> List.member location locations
    _ -> False
  
gameState : GameBoardEx -> GameState
gameState boardEx =
  let
    blackWinner = Dict.toList boardEx.counters |> List.filter (\(f, (b,w)) -> b >= 5) |> List.head
    whiteWinner = Dict.toList boardEx.counters |> List.filter (\(f, (b,w)) -> w >= 5) |> List.head
  in 
    case (blackWinner) of
      Just c -> Winner Black (c |> Tuple.first |> locationsForFive)
      Nothing ->
        case (whiteWinner) of
          Just c  -> Winner White (c |> Tuple.first |> locationsForFive)
          Nothing -> 
            if (List.isEmpty boardEx.moves) then
              Draw
            else
              GameInProgress

board : GameBoardEx -> GameBoard
board gameEx =
  gameEx.board

counters : GameBoardEx -> BoardCounter
counters gameEx =
  gameEx.counters

moves : GameBoardEx -> List GameMove
moves gameEx =
  gameEx.moves

value : GameBoardEx -> (Vector5, Vector5)
value gameEx =
  gameEx.values

player : GameBoardEx -> Color
player gameEx =
  gameEx.player

-------------------------------------------------------------------------------
-- GAMEBOARD FUNCTIONS
-------------------------------------------------------------------------------

{-| Represent direction. 
-}
type alias Direction
  = (Int, Int)

{-| A five can be represented by two values: start location and direction 
-}
type alias Five 
  = (Location, Direction)

{-| Returns all possible directions.
-}
allDirections : List Direction
allDirections =
  [(0,1),(1,0),(1,1),(1,-1)]

affectedFives : Location -> List Five
affectedFives loc =
  allDirections
    |> List.map (\dir -> fivesInALine loc dir)
    |> List.concat

fivesInALine : Location -> Direction -> List Five
fivesInALine (x, y) (dx, dy) =
  [ (((x-4*dx),(y-4*dy)),(dx,dy))
  , (((x-3*dx),(y-3*dy)),(dx,dy))
  , (((x-2*dx),(y-2*dy)),(dx,dy))
  , (((x-1*dx),(y-1*dy)),(dx,dy))
  , (((x+0*dx),(y+0*dy)),(dx,dy))
  ]

locationsForFive : Five -> List Location
locationsForFive ((x,y),(dx,dy)) =
  [(x,y), (x+dx,y+dy), (x+2*dx,y+2*dy), (x+3*dx,y+3*dy), (x+4*dx,y+4*dy)]

neighbors : Location -> List Location
neighbors (r,c) =
  [(r-1, c-1),(r, c-1),(r+1, c-1),(r-1, c),(r+1, c),(r-1, c+1),(r, c+1),(r+1, c+1)]

mapBoardFields : GameBoardEx -> ((Int, Int) -> Field -> a) -> List a
mapBoardFields game mapFunc =
  game.board
    |> Matrix.mapWithLocation mapFunc
    |> Matrix.flatten

putStone : Location -> Color -> GameBoardEx -> GameBoardEx
putStone loc color boardEx =
  { boardEx | board = Matrix.set loc (Stone color) boardEx.board }

get : Location -> GameBoard -> Field
get location board =
  case (Matrix.get location board) of
    Nothing -> Empty
    Just x -> x

-------------------------------------------------------------------------------
-- ALGORITHM DEFINITION FUNCTIONS
-------------------------------------------------------------------------------

{-| Represent evaluation of the recognized patterns like 
singles, doubles, triples, quarters and a five.
-}
type alias HeuristicRatings = Vector5

{-| Represent algorithm parameters. 

    depth - maximum depth for minimax algorithm.
    attackRatings - evaluation of recognized patters for attack.
    defenseRatings - evaluation of recognized patterns for defense.
-}
type alias AlgParams = 
  { depth : Int
  , attackRatings : HeuristicRatings
  , defenseRatings : HeuristicRatings
  }

basic : AlgParams
basic =
  createAlgParams 1 defaultAttackRatings defaultDefenseRatings

advanced : AlgParams
advanced =
  createAlgParams 2 defaultAttackRatings defaultDefenseRatings

expert : AlgParams 
expert =
  createAlgParams 3 defaultAttackRatings defaultDefenseRatings

superexpert : AlgParams 
superexpert =
  createAlgParams 4 defaultAttackRatings defaultDefenseRatings

defaultAttackRatings : HeuristicRatings
defaultAttackRatings =
  (1,3,100,500,8000)

defaultDefenseRatings : HeuristicRatings
defaultDefenseRatings =
  (2,20,90,700,10000)
 
createAlgParams : Int -> HeuristicRatings -> HeuristicRatings -> AlgParams
createAlgParams depth attackRatings defenseRatings =
  { depth = depth
  , attackRatings = attackRatings
  , defenseRatings = defenseRatings
  }

-------------------------------------------------------------------------------
-- HELPER FUNCTIONS FOR COMPUTING WITH VECTOR5
-------------------------------------------------------------------------------

{-| Alias for five-dimensional vector of the integers.
-}
type alias Vector5 = (Int, Int, Int, Int, Int)

superSum : (Vector5, Vector5) -> (Vector5, Vector5) -> (Vector5, Vector5)
superSum (a,b) (c,d) =
  ((sum a c), (sum b d))

sum : Vector5 -> Vector5 -> Vector5
sum (a,b,c,d,e) (v,w,x,y,z) =
  (a+v, b+w, c+x, d+y, e+z)

scalar : Vector5 -> Vector5 -> Int
scalar (a,b,c,d,e) (v,w,x,y,z) =
  (a*v + b*w + c*x + d*y + e*z)

-------------------------------------------------------------------------------
-- HELPER PRINTING FUNCTIONS
-------------------------------------------------------------------------------

prettyPrint : GameBoard -> String
prettyPrint matrix =
  "\n   0 1 2 3 4 5 6 7 8 9 10" ++
  (Matrix.mapWithLocation printField matrix
    |> Matrix.flatten
    |> String.concat)

printField : Location -> Field -> String
printField (row, col) field =
  let
    pad = if (col == 10) then
       ""
       else
        " "

    f =  if (col == 0) then
            "\n" ++ (toString row) ++ pad ++ " "
          else
            " "
  in
    case field of
      Empty -> f ++ "_"
      Stone Black -> f ++ "x"
      Stone White -> f ++ "o"

-------------------------------------------------------------------------------
-- HELPER LIST FUNCTIONS
-------------------------------------------------------------------------------

unique : List comparable -> List comparable
unique list =
    uniqueHelp identity Set.empty list

uniqueHelp : (a -> comparable) -> Set comparable -> List a -> List a
uniqueHelp f existing remaining =
    case remaining of
        [] ->
            []

        first :: rest ->
            let
                computedFirst =
                    f first
            in
                if Set.member computedFirst existing then
                    uniqueHelp f existing rest
                else
                    first :: uniqueHelp f (Set.insert computedFirst existing) rest