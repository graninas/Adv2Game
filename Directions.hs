module Directions where

import Types

isExistPaths :: Paths -> Direction -> Bool
isExistPaths [] _ = False
isExistPaths (p:ps) dir = if pathDir p == dir then True else isExistPaths ps dir