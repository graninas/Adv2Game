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

type InputString = String
type OutputMessage = String

data GameAction =
				PrintMessage OutputMessage
				| QuitGame OutputMessage
				| ReadUserInput
				| ReadMessagedUserInput OutputMessage
				| SaveState GameState OutputMessage

run' :: InputString -> GameState -> GameAction
run' inputStr curGS = do
		let currentRoom = gsCurrentRoom curGS
		let roomObjects = locationObjects (gsLocations curGS) currentRoom
		let inventory = gsInvObjects curGS
		case parseCommand inputStr of
			(Nothing, []) -> ReadUserInput
			(Nothing, str) -> PrintMessage str
			(Just Quit, _) -> QuitGame "Be seen you..."
			(Just (Walk dir), _) -> tryWalk dir curGS
			(Just Inventory, _) -> PrintMessage (showInventory inventory)
			(Just Look, _) -> PrintMessage (lookAround currentRoom roomObjects)
			(Just (Investigate itmName), _) -> tryInvestigateItem itmName roomObjects inventory
			(Just (Pickup itmName), _) -> tryPickup itmName curGS

run :: InputString -> GS ()
run inputStr = do
	gameAction <- run' inputStr get
	case gameAction of
		QuitGame outMsg -> ioOutMsgGS outMsg >> return ()
		PrintMessage outMsg -> ioOutMsgGS outMsg >> run ""
		ReadUserInput -> ioInMsgGS >>= run
		ReadMessagedUserInput outMsg -> ioOutMsgGS outMsg >> ioInMsgGS >>= run
		SaveState newState outMsg -> ioOutMsgGS outMsg >> put newState >> run ""

main :: IO ()
main = do
	putStrLn $ lookAround startRoom startRoomObjects
	x <- evalStateT (runGameState (run "")) initWorld
	putStrLn ""
		where
			startRoom = gsCurrentRoom $ initWorld
			startRoomObjects = locObjects . location $ startRoom