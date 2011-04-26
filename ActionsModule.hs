module ActionsModule where

import Types
import Locations
import Tools
import DirectionsModule

actionFromStr :: String -> Action
actionFromStr x = case upString(x) of
						"WALK" -> Walk
						"LOOK" -> Look
						"GO" -> Walk
						"QUIT" -> Quit
						"Q" -> Quit
						_ -> NoAction

parseAction :: [String] -> Direction -> Action
parseAction [] _ = NoAction
parseAction _ x | x /= NoDirection = Walk
parseAction (x:xs) _ = case (action) of
							NoAction -> parseAction xs NoDirection
							otherwise -> action
						where action = actionFromStr x
