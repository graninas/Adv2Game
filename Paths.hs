module Paths where

import Types
import Text.Printf(printf)

pathOnDirection :: Paths -> Direction -> Maybe Path
pathOnDirection ps dir = case filter (\x -> pathDirection x == dir) ps of
					[] -> Nothing
					(x:xs) -> Just x