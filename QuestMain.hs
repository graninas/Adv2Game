module Main where

import Types
import Locations
import DirectionsModule
import ActionsModule
import Tools

parseStrToCommand :: String -> IO Command
parseStrToCommand [] = return (Command NoAction NoDirection)
parseStrToCommand x = do
						let commandWords = words x
						let direction = parseDirection commandWords
						let action = parseAction commandWords direction
						return (Command action direction)

describeGameSituation :: GameSituation -> IO ()
describeGameSituation gameSit =
			case locDescr of
				Just locDescr -> do
								putStrLn locDescr
				Nothing -> return ()
			where
				isShort = gameDescribeShort $ gameSit
				isLong = gameDescribeLong $ gameSit
				room = gameRoom $ gameSit
				locDescr = describeLocation room isShort isLong

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
				putStrLn . show . commandAction $ c -- В отладочных целях
				putStrLn . show . commandDir $ c -- В отладочных целях
				case commandAction c of
					Quit -> return ()
					otherwise -> do
						newGameSituation <- getNewGameSituation (gameRoom $ oldGameSituation) c
						run newGameSituation


main :: IO ()
main = run (GameSituation SouthRoom True False)