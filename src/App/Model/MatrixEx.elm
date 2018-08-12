module App.Model.MatrixEx exposing (findAllSeries, findSeries, generateLine, matchAll)

import App.Model.Serie as Serie exposing (Serie(..))

import Matrix exposing (Matrix, Location, get, set, loc, rowCount, colCount, flatten)

{-| Represent direction. 
-}
type alias Vector
  = (Int, Int)

{-|
-}
findAllSeries : Matrix a -> List (Serie a)
findAllSeries matrix =
  let
    cols = colCount matrix
    rows = rowCount matrix
    
    topLine    = generateLine (0,1) cols (0,0) 
    leftLine   = generateLine (1,0) rows (0,0) 
    rightLine  = generateLine (1,0) rows (0, cols-1) 

    lines = [topLine, leftLine, topLine, leftLine, topLine, rightLine]
    vectors = [(1,0), (0,1), (1,1), (1,1), (1,-1), (1,-1)]

    func =
      \matrix vector line -> 
        line 
          |> List.map (findSeries matrix vector)
          |> List.concat
          |> List.filter (\n -> Serie.length n > 0)
  in 
    List.map2 (func matrix) vectors lines |> List.concat

{-|
-}
findSeries : Matrix a -> Vector -> Location -> (List (Serie a))
findSeries  matrix vector startLoc =
  findSeries_ matrix vector ([], EmptySerie) startLoc |> Tuple.first

{-|
-}
findSeries_ : Matrix a -> Vector -> (List (Serie a), Serie a) -> Location -> (List (Serie a), Serie a)
findSeries_ matrix vector (found, current) startLoc =
  let
    item = Matrix.get startLoc matrix
    nextLoc = moveLoc_ startLoc vector
  in
    case item of
      Nothing ->
        (current :: found, EmptySerie)

      Just a ->
        if (Serie.include current a) then
          findSeries_ matrix vector (found, Serie.append current a startLoc) nextLoc
        else
          findSeries_ matrix vector (current :: found, Serie.create a startLoc) nextLoc

{-|
-}
generateLine :  Vector -> Int -> Location -> List Location
generateLine vector length start =
  if (length <= 1) then
    [start]
  else
    start :: (generateLine vector (length-1) (moveLoc_ start vector))

{-|
-}
matchAll : Matrix a -> List Location -> Bool
matchAll matrix locations =
  let
    rows = Matrix.rowCount matrix
    cols = Matrix.colCount matrix
  in
  case locations of
    [] -> True
    ((r,c) :: remains) -> (r >= 0) && (r < rows) && (c >= 0) && (c < cols) && (matchAll matrix remains)


all_ : List a -> List b -> List (a, b)
all_ la lb =
  case la of
    [] -> []
    (x :: tail) -> List.append (List.map (\n -> (x, n)) lb) (all_ tail lb)


{-|
-}
moveLoc_ : Location -> Vector -> Location
moveLoc_ (r,c) (dr,dc) =
  ((r+dr), (c+dc))
