module Main where

import Types
import Locations
import DirectionsModule
import Tools
					
getCommand :: String -> Command
getCommand = undefined

describeActions :: Room -> String
describeActions _ = "You can do something."

describeGameSituation :: Room -> IO ()
describeGameSituation room = do
						putStrLn . describeDirections $ room
						putStrLn . describeActions $ room
						
canWalk :: Room -> Direction -> Bool
canWalk room dir = dir `elem` (getRoomDirections room)

getNewGameState :: Room -> String -> Room
getNewGameState oldRoom x = case actionCommand $ command of
								Walk -> if canWalk oldRoom dir then walkBy dir else undefined
								Look -> undefined
							where
								command = getCommand x
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