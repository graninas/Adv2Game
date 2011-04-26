module Main where

import Types
import Locations
import DirectionsModule
import Tools
					
parseStrToCommand :: String -> IO Command
parseStrToCommand "" = return (Command NoAction NoDirection)
parseStrToCommand "Q" = return (Command Quit NoDirection)
parseStrToCommand x | isWalkAction x = return (Command Walk (directionFromStrCommand x))
					| otherwise = undefined

describeActions :: Room -> String
describeActions _ = "You can do something."

describeGameSituation :: Room -> IO ()
describeGameSituation room = do
						putStrLn . describeDirections $ room
						putStrLn . describeActions $ room
						


getNewGameState :: Room -> Command -> Room
getNewGameState oldRoom command = undefined


run :: Room -> IO ()
run oldRoom = do
				x <- inputStrCommand
				c <- parseStrToCommand (upString x)
				case commandAction c of
					Quit -> return ()
					otherwise -> run (getNewGameState oldRoom c)
									

main :: IO ()
main = do
		describeGameSituation SouthRoom
		run SouthRoom