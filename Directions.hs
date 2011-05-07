module Directions where

import Types
import Text.Printf(printf)

roomOnDirection :: Paths -> Direction -> Maybe Room
roomOnDirection [] _ = Nothing
roomOnDirection (p:ps) dir = if pathDir p == dir then Just . pathRoom $ p else roomOnDirection ps dir

failureWalkingMsg :: Direction -> String
failureWalkingMsg dir = printf "You can't walk to %s." (show dir)