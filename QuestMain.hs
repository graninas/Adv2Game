module Main where

import Types
import Locations
import Directions
import Objects
import Tools
import Control.Monad.State (get, gets, StateT(..), evalStateT, 
                            put, MonadState(..), liftIO)
							


parseCommand :: String -> (Maybe Command, String)
parseCommand [] = (Nothing, [])
parseCommand str = case reads capStrings of
					[(x,"")] -> (Just x, [])
					_ -> case head capStrings of
						'Q' -> (Just Quit, "Be seen you...")
						'P' -> case reads wordsAfterCommand of
							[(y,"")] -> (Just (Pickup y), [])
							_ -> (Nothing, "Pickup what?")
						_ -> (Nothing, "Can't understand a command.")
						where wordsAfterCommand = unwords . tail . words $ capStrings
	where capStrings = capitalize $ str

newGameState :: WorldMap -> Room -> LongDescribedRooms -> Inventory -> GameState
newGameState newWorldMap newRoom newLongDescribedRooms newInventory = GameState {
	gsWorldMap = newWorldMap,
	gsCurrentRoom = newRoom,
	gsRoomLongDescribed = newLongDescribedRooms,
	gsInventory = newInventory}

canWalk :: GameState -> Direction -> Maybe Room
canWalk curGS = roomOnDirection (locPaths . location . gsCurrentRoom $ curGS)

tryWalk dir curGS = do
	case canWalk curGS dir of
		Just room -> do
			put (newGameState (gsWorldMap curGS) room newLongDescribedRooms (gsInventory curGS))
			ioOutMsg $ (describeLocation roomAlreadyLongDescribed room (locationObjects (gsWorldMap curGS) room))
			return ContinueGame
				where
					roomsDescribedEarlier = gsRoomLongDescribed curGS
					roomAlreadyLongDescribed = isRoomLongDescribed roomsDescribedEarlier room
					newLongDescribedRooms = if roomAlreadyLongDescribed then roomsDescribedEarlier else room : roomsDescribedEarlier
		Nothing -> return ContinueGame

tryPickup obj curGS = do
	case tryRiseObject obj of
		(Nothing, str) -> (ioOutMsg $ str) >> return ContinueGame
		(Just x, _) -> do
			put (newGameState (gsWorldMap curGS) (gsCurrentRoom curGS) (gsRoomLongDescribed curGS) (addToInventory (gsInventory curGS) obj))
			return ContinueGame
	

run :: GS Result
run = do
	curGS <- get
	strCmd <- liftIO inputStrCommand
	let parsedCmdWithContext = parseCommand strCmd
	let currentRoom = gsCurrentRoom $ curGS
	let roomObjects =  locationObjects (gsWorldMap curGS) currentRoom
	let inventory = gsInventory curGS
	case parsedCmdWithContext of
		(Nothing, str) -> (ioOutMsg $ str) >> run
		(parsedCmd, str) -> case parsedCmd of
			Just Quit -> ioOutMsg str >> return QuitGame
			Just Look -> ioOutMsg (lookAround currentRoom roomObjects) >> run
			Just (Go dir) -> (tryWalk dir curGS) >> run
			Just (Walk dir) -> (tryWalk dir curGS) >> run
			Just (Pickup obj) -> if canSeeObject obj roomObjects then (tryPickup obj curGS) >> run else (ioOutMsg . notVisibleObjectError $ obj) >> run
			Nothing -> (ioOutMsg . show $ parsedCmd) >> run

main :: IO ()
main = do
	putStrLn $ lookAround startRoom startRoomObjects
	x <- evalStateT (runGameState run) initWorld
	putStrLn ""
		where
			startRoom = gsCurrentRoom $ initWorld
			startRoomObjects = locObjects . location $ startRoom