module Directions where

import Types
import Text.Printf(printf)

roomOnDirection :: Paths -> Direction -> Maybe Room
roomOnDirection [] _ = Nothing
roomOnDirection (p:ps) dir = if pathDirection p == dir then Just . pathRoom $ p else roomOnDirection ps dir

successWalkingMsg :: Room -> Direction -> String
successWalkingMsg room dir = printf "You walking %s to %s."  (show dir) (show room)

failureWalkingMsg :: Direction -> String
failureWalkingMsg dir = printf "You can't walk %s." (show dir)