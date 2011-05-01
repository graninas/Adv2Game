module Directions where

import Types

roomOnDirection :: Paths -> Direction -> Maybe Room
roomOnDirection [] _ = Nothing
roomOnDirection (p:ps) dir = if pathDir p == dir then Just . pathRoom $ p else roomOnDirection ps dir