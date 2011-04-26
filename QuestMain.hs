module Main where

import Types
import Locations
import DirectionsModule
import ActionsModule
import Tools
					
parseStrToCommand :: String -> IO Command
parseStrToCommand "" = return (Command Look NoDirection)
parseStrToCommand x = do
						let commandWords = words x
						let direction = parseDirection commandWords
						let action = parseAction commandWords direction
						return (Command action direction)

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
				c <- parseStrToCommand x
				putStrLn . show . commandAction $ c
				putStrLn . show . commandDir $ c
				case commandAction c of
					Quit -> return ()
					otherwise -> run (getNewGameState oldRoom c)
									

main :: IO ()
main = do
		describeGameSituation SouthRoom
		run SouthRoom