module Main where

import Types
import Locations
import DirectionsModule
import Tools
					
parseStrToCommand :: String -> Command
parseStrToCommand "" = Command Look NoDirection

describeActions :: Room -> String
describeActions _ = "You can do something."

describeGameSituation :: Room -> IO ()
describeGameSituation room = do
						putStrLn . describeDirections $ room
						putStrLn . describeActions $ room
						


getNewGameState :: Room -> String -> Room
getNewGameState oldRoom x = case actionCommand $ command of
								Walk -> if canWalk oldRoom dir then walkToDir oldRoom dir else undefined
								Look -> undefined
							where
								command = parseStrToCommand x
								dir = dirCommand $ command


run :: Room -> IO ()
run oldRoom = do
				describeGameSituation oldRoom
				x <- inputStrCommand
				case x of
					"Quit" -> return ()
					"q" -> return ()
					otherwise -> run (getNewGameState oldRoom x)

main :: IO ()
main = run SouthRoom