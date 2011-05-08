module Main where

import Types
import Objects
import Tools
import GameState
import Locations
import Control.Monad.State (get, gets, StateT(..), evalStateT, 
                            put, MonadState(..), liftIO)
import Char(isDigit, digitToInt)


parseCommand [] = (Nothing, [])
parseCommand str = case reads capStrings of
					[(x,"")] -> (Just x, [])
					_ -> case head capStrings of
						'Q' -> (Just Quit, "Be seen you...")
						'I' -> (Just Inventory, [])
						'P' -> case reads wordsAfterCommand of
							[(y, "")] -> (Just (Pickup y), [])
							_ -> (Nothing, "Pickup what?")
						_ -> (Nothing, "Can't understand a command.")
						where wordsAfterCommand = unwords . tail . words $ capStrings
	where capStrings = capitalize $ str

parseObject :: String -> Objects -> Maybe Object
parseObject _ [] = Nothing
parseObject str objects = case read str of
						[(x, "")] -> case isDigit x of
							True -> Just ( objects!!((digitToInt x)-1) )
							False -> Nothing

run' :: String -> Bool -> GS GameActionResult
run' msg qualifiedInput = do
		curGS <- get
		let currentRoom = gsCurrentRoom curGS
		let roomObjects = locationObjects (gsLocations curGS) currentRoom
		let inventory = gsInvObjects curGS
		let pickupRoutine itmName = do
				(pickupingMsg, maybeNewState, needSelectObject) <- tryPickup itmName curGS
				case needSelectObject of
					False -> return (SaveState, pickupingMsg, maybeNewState)
					True ->  return (ReadUserInputForPickup, pickupingMsg, Nothing)
		case qualifiedInput of
			False -> case parseCommand msg of
					(Nothing, []) -> return (ReadUserInput, [], Nothing)
					(Nothing, str) -> return (PrintMessage, str, Nothing)
					(Just Quit, _) -> return (QuitGame, "Be seen you...", Nothing)
					(Just (Walk dir), _) -> do
										(walkMsg, newState) <- tryWalk dir curGS
										return (SaveState, walkMsg, newState)
					(Just Inventory, _) -> return (PrintMessage, showInventory inventory, Nothing)
					(Just Look, _) -> return (PrintMessage, lookAround currentRoom roomObjects, Nothing)
					(Just (Investigate itmName), _) -> case canSeeObject (roomObjects ++ inventory) itmName of
							True -> return (PrintMessage, investigateObject itmName (roomObjects ++ inventory), Nothing)
							False -> return (PrintMessage, notVisibleObjectError itmName, Nothing)
					(Just (Pickup itmName), _) -> pickupRoutine itmName
			--True ->
		


run :: String -> GS ()
run msg = do
	(gameAction, str, maybeGameState) <- run' msg False
	case gameAction of
		QuitGame -> ioOutMsgGS str >> return ()
		PrintMessage -> ioOutMsgGS str >> run ""
		ReadUserInput -> ioInMsgGS >>= run
		SaveState -> case maybeGameState of
				Just newState -> ioOutMsgGS str >> put newState >> run ""
				Nothing -> ioOutMsgGS str >> run ""
		ReadUserInputForPickup -> ioInMsgGS >>= run

main :: IO ()
main = do
	putStrLn $ lookAround startRoom startRoomObjects
	x <- evalStateT (runGameState (run [])) initWorld
	putStrLn ""
		where
			startRoom = gsCurrentRoom $ initWorld
			startRoomObjects = locObjects . location $ startRoom