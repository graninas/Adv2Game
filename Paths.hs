module Paths where

import Types
import Text.Printf(printf)

getPathOnDirection :: Paths -> Direction -> Maybe Path
getPathOnDirection ps dir = case filter (\x -> pathDirection x == dir) ps of
					[] -> Nothing
					(x:xs) -> Just x

successWalkingMsg :: Room -> Direction -> String
successWalkingMsg room dir = printf "You walking %s to %s."  (show dir) (show room)

failureWalkingMsg :: Direction -> String
failureWalkingMsg dir = printf "You can't walk %s." (show dir)