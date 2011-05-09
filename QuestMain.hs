module Main where

import Types
import Objects
import Tools
import GameState
import Locations
import Control.Monad.State (get, gets, StateT(..), evalStateT, 
                            put, MonadState(..), liftIO)
import Char(isDigit, digitToInt)


parseCommand :: String -> ParseResult
parseCommand [] = (Nothing, [])
parseCommand str = case reads capStrings of
					[(x,"")] -> (Just x, [])
					_ -> case head capedWords of
						"Q" -> (Just Quit, "Be seen you...")
						"I" -> (Just Inventory, [])
						"P" -> case wordsAfterCommand of
							[] -> (Nothing, "Pickup what?")
							otherwise -> case reads wordsAfterCommand of
										[(y, "")] -> (Just (Pickup y), [])
										_ -> (Nothing, "Can't understand a command.")
						_ -> (Nothing, "Can't understand a command.")
	where
		capStrings = capitalize $ str
		capedWords = words capStrings
		wordsAfterCommand = unwords . tail $ capedWords

run' :: InputString -> Maybe InputCommand -> GameState -> GameAction
run' inputStr maybeInputCmd curGS = do
		let currentRoom = gsCurrentRoom curGS
		let roomObjects = locationObjects (gsLocations curGS) currentRoom
		let inventory = gsInvObjects curGS
		case maybeInputCmd of
			Nothing -> case parseCommand inputStr of
				(Nothing, []) -> ReadUserInput
				(Nothing, str) -> PrintMessage str
				(Just Quit, _) -> QuitGame "Be seen you..."
				(Just (Walk dir), _) -> tryWalk dir curGS
				(Just Inventory, _) -> PrintMessage (showInventory inventory)
				(Just Look, _) -> PrintMessage (lookAround currentRoom roomObjects)
				(Just (Investigate itmName), _) -> tryInvestigateItem itmName (roomObjects ++ inventory)
				(Just (Pickup itmName), _) -> tryPickup itmName roomObjects curGS
				(Just (Take str), _) -> undefined
			Just (QualifyPickup objects) -> tryTake inputStr objects curGS

run :: InputString -> Maybe InputCommand -> GS ()
run inputStr oldInputCmd = do
	curGS <- get
	gameAction <- return (run' inputStr oldInputCmd curGS)
	case gameAction of
		QuitGame outMsg -> ioOutMsgGS outMsg >> return ()
		PrintMessage outMsg -> ioOutMsgGS outMsg >> run "" Nothing
		ReadUserInput -> ioInMsgGS >>= \x -> run x Nothing
		ReadMessagedUserInput inOutString newInputCmd -> ioOutMsgGS inOutString >> ioInMsgGS >>= \x -> run x (Just newInputCmd)
		SaveState newState outMsg -> ioOutMsgGS outMsg >> put newState >> run "" Nothing

main :: IO ()
main = do
	putStrLn $ lookAround startRoom startRoomObjects
	x <- evalStateT (runGameState (run "" Nothing)) initWorld
	putStrLn ""
		where
			startRoom = gsCurrentRoom $ initWorld
			startRoomObjects = locObjects . location $ startRoom