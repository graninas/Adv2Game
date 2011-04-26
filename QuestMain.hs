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

getNewGameSituation :: Room -> Command -> IO Room						
getNewGameSituation room command = do
									let direction = commandDir $ command
									return (walkToDir room direction)

run :: Room -> IO ()
run oldRoom = do
				x <- inputStrCommand
				c <- parseStrToCommand x
				putStrLn . show . commandAction $ c
				putStrLn . show . commandDir $ c
				case commandAction c of
					Quit -> return ()
					otherwise -> do
						newRoom <- getNewGameSituation oldRoom c
						run newRoom


main :: IO ()
main = do
		describeGameSituation SouthRoom
		run SouthRoom