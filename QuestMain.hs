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

describeGameSituation :: GameSituation -> IO ()
describeGameSituation gameSit = putStrLn (describeLocation (gameRoom $ gameSit) (gameDescribeShort $ gameSit) (gameDescribeLong $ gameSit))
									

getNewGameSituation :: Room -> Command -> IO GameSituation
getNewGameSituation room command = do
									let direction = commandDir $ command
									let nextRoom = walkToDir room direction
									let describeShort = nextRoom /= room
									let describeLong = (commandAction $ command) == Look
									return (GameSituation nextRoom describeShort describeLong)

run :: GameSituation -> IO ()
run oldGameSituation = do
				describeGameSituation oldGameSituation
				x <- inputStrCommand
				c <- parseStrToCommand x
				putStrLn . show . commandAction $ c
				putStrLn . show . commandDir $ c
				case commandAction c of
					Quit -> return ()
					otherwise -> do
						newGameSituation <- getNewGameSituation (gameRoom $ oldGameSituation) c
						run newGameSituation


main :: IO ()
main = run (GameSituation SouthRoom True False)