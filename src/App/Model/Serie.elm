module App.Model.Serie exposing (Serie(..), create, append, length, include, member)

import Matrix exposing (Location)

{-| Represent some serie of locations assoicated to a item.
-}
type Serie a
  = EmptySerie | FullSerie a (List Location)

{-|
-}
create : a -> Location -> Serie a
create a loc =
  FullSerie a [loc]

{-|
-}
append : Serie a -> a -> Location -> Serie a
append serie a loc =
  case serie of
    EmptySerie -> 
      FullSerie a [loc]

    FullSerie c l -> 
      FullSerie c (loc :: l)

{-|
-}
length : Serie a -> Int
length serie =
  case serie of
    EmptySerie -> 0
    FullSerie _ l -> List.length l

{-|
-}
include : Serie a -> a -> Bool
include serie a =
  case serie of
    EmptySerie -> False
    FullSerie serieA _ -> a == serieA

{-|
-}
member : Location -> Serie a -> Bool
member location serie =
  case serie of
    EmptySerie -> False
    FullSerie _ locations -> List.member location locations
