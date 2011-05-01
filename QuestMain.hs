module Main where

import Types
import Locations
import Directions
import Objects
import Tools
import Control.Monad.State (get, gets, StateT(..), evalStateT, 
                            liftIO, put, MonadState(..), MonadIO(..))

parseCommand :: String -> Maybe Command
parseCommand [] = Nothing
parseCommand str = case reads capStrings of
					[(x,"")] -> Just x
					_ -> case head capStrings of
						'Q' -> Just Quit
						'P' -> case reads wordsAfterCommand of
							[(y,"")] -> Just (Pickup y)
							_ -> Nothing
						_ -> Nothing
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
			liftIO $ putStrLn $ (describeLocation roomAlreadyLongDescribed room (locationObjects (gsWorldMap curGS) room))
			return ContinueGame
				where
					roomsDescribedEarlier = gsRoomLongDescribed curGS
					roomAlreadyLongDescribed = isRoomLongDescribed roomsDescribedEarlier room
					newLongDescribedRooms = if roomAlreadyLongDescribed then roomsDescribedEarlier else room : roomsDescribedEarlier
		Nothing -> return ContinueGame

tryPickup obj curGS = do
	case tryRiseObject obj of
		(Nothing, str) -> (liftIO . putStrLn $ str) >> return ContinueGame
		(Just x, _) -> do
			put (newGameState (gsWorldMap curGS) (gsCurrentRoom curGS) (gsRoomLongDescribed curGS) (addToInventory (gsInventory curGS) obj))
			return ContinueGame
	

run :: GS Result
run = do
	curGS <- get
	strCmd <- liftIO inputStrCommand
	let parsedCmd = parseCommand strCmd
	let currentRoom = gsCurrentRoom $ curGS
	let roomObjects =  locationObjects (gsWorldMap curGS) currentRoom
	let inventory = gsInventory curGS
	case parsedCmd of
		Just Quit -> return QuitGame
		Just Look -> liftIO (putStrLn (lookAround currentRoom roomObjects)) >> run
		Just (Go dir) -> (tryWalk dir curGS) >> run
		Just (Walk dir) -> (tryWalk dir curGS) >> run
		Just (Pickup obj) -> if canSeeObject obj roomObjects then (tryPickup obj curGS) >> run else (liftIO . putStrLn . notVisibleObjectError $ obj) >> run
		Nothing -> (liftIO . putStrLn . show $ parsedCmd) >> run

main :: IO ()
main = do
	putStrLn $ lookAround startRoom startRoomObjects
	x <- evalStateT (runGameState run) initWorld
	putStrLn "End."
		where
			startRoom = gsCurrentRoom $ initWorld
			startRoomObjects = locObjects . location $ startRoom