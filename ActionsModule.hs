module ActionsModule where

import Types
import Locations
import Tools

actionFromStr :: String -> Action
actionFromStr x = case upString(x) of
						"WALK" -> Walk
						"LOOK" -> Look
						"GO" -> Walk
						"QUIT" -> Quit
						_ -> NoAction

parseAction :: [String] -> Action
parseAction [] = NoAction
parseAction (x:xs) =	case (action) of
							NoAction -> parseAction xs
							otherwise -> action
						where action = actionFromStr x
