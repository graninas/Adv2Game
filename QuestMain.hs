module Main where

import Types
import Locations
import Directions
import Objects
import Tools
import Control.Monad.State (get, gets, StateT(..), evalStateT, 
                            put, MonadState(..), liftIO)
import Char(isDigit, digitToInt)


parseCommand :: String -> (Maybe Command, String)
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
	
	
newGameState :: Locations -> Room -> LongDescribedRooms -> InventoryObjects -> GameState
newGameState newLocations newRoom newLongDescribedRooms newInventory = GameState {
	gsLocations = newLocations,
	gsCurrentRoom = newRoom,
	gsRoomLongDescribed = newLongDescribedRooms,
	gsInvObjects = newInventory}

canWalk :: GameState -> Direction -> Maybe Room
canWalk = roomOnDirection . locPaths . location . gsCurrentRoom

tryWalk :: Direction -> GameState -> GS Result
tryWalk dir curGS = case canWalk curGS dir of
		Just room -> do
			put (newGameState (gsLocations curGS) room newLongDescribedRooms (gsInvObjects curGS))
			ioOutMsgGS $ (describeLocation roomAlreadyLongDescribed room (locationObjects (gsLocations curGS) room))
			return ContinueGame
				where
					roomsDescribedEarlier = gsRoomLongDescribed curGS
					roomAlreadyLongDescribed = isRoomLongDescribed roomsDescribedEarlier room
					newLongDescribedRooms = if roomAlreadyLongDescribed then roomsDescribedEarlier else room : roomsDescribedEarlier
		Nothing -> return ContinueGame


{-
	
saveInventoryObject :: Object -> GameState -> GS Result
saveInventoryObject obj curGS = put (newGameState (locationsWithoutObject curLocations curRoom obj) curRoom curRoomLongDescribed (obj : curInventory))
								>> return ContinueGame
	where
		curLocations = gsLocations curGS
		curRoom = gsCurrentRoom curGS
		curInventory = gsInvObjects curGS
		curRoomLongDescribed = gsRoomLongDescribed curGS
		curLocObjects = locObjects' curRoom curLocations
		locObjects' room locs = locObjects . head $ (filter (\y -> room == locRoom y) locs)

exactlyObject :: Objects -> Maybe Object
exactlyObject [] = Nothing
exactlyObject (x:[]) = Just x
exactlyObject xs = undefined

tryPickup' :: ItemName -> Objects -> GameState -> GS Result
tryPickup' itemNme locObjects curGS = saveState . maybeObject $ mathedObjects itemNme locObjects
	where
		maybeObject [] = Nothing
		maybeObject (o:[]) = Just o
		maybeObject (os) = do
			_ <- return (ioOutMsgGS (enumerateObjects "What object of these variants: " os))
			parsedObj <- return (inputStrCommand \s -> (parseObject s locObjects))
			Nothing
		saveState mbObj = case mbObj of
			Nothing -> return ContinueGame
			Just obj -> saveInventoryObject obj curGS

-}
{-tryPickup itemNme curGS = do
	case exactlySelectedObject of
		Nothing -> ioOutMsgGS "Ok." >> return ContinueGame
		Just obj ->	case tryRiseObject obj of
			(Nothing, str) -> (ioOutMsgGS $ str) >> return ContinueGame
			(Just x, str) -> do
				(ioOutMsgGS $ str)
				put (newGameState (locationsWithoutObject curLocStates curRoom obj) curRoom curRoomLongDescribed (obj : curInventory))
				return ContinueGame
	where
		curLocStates = gsLocations curGS
		curRoom = gsCurrentRoom curGS
		curInventory = gsInvObjects curGS
		curRoomLongDescribed = gsRoomLongDescribed curGS
		curLocObjects = locObjects' curRoom curLocStates
		locObjects' room locs = locObjects . head $ (filter (\y -> room == locRoom y) locs)
		objList = objectListFromObjectsByItemName itemNme curLocObjects
		exactlySelectedObject = if length objList == 1 then head objList else whatObjectExactly (liftIO inputStrCommand) objList
						--_ <- ioOutMsgIO (enumObjects "What object of these variants: " objList)
-}				

run :: GS Result
run = do
	curGS <- get
	strCmd <- liftIO inputStrCommand
	let parsedCmdWithContext = parseCommand strCmd
	let currentRoom = gsCurrentRoom $ curGS
	let roomObjects = locationObjects (gsLocations curGS) currentRoom
	let inventory = gsInvObjects curGS
	case parsedCmdWithContext of
		(Nothing, str) -> (ioOutMsgGS $ str) >> run
		(parsedCmd, str) -> case parsedCmd of
			Just Quit -> ioOutMsgGS str >> return QuitGame
			Just Look -> ioOutMsgGS (lookAround currentRoom roomObjects) >> run
			Just Inventory -> (ioOutMsgGS . showInventory $ inventory) >> run
			Just (Investigate itemNme) ->
				if canSeeObj itemNme
				then invObj >> run
				else (noVisObjMsg itemNme) >> run
					where invObj = ioOutMsgGS  (investigateObject itemNme (roomObjects ++ inventory))
			Just (Go dir) -> (tryWalk dir curGS) >> run
			Just (Walk dir) -> (tryWalk dir curGS) >> run
			Just (Pickup itemNme) -> if canSeeObj itemNme then (tryPickup' itemNme roomObjects curGS) >> run else (noVisObjMsg itemNme) >> run
			Nothing -> (ioOutMsgGS . show $ parsedCmd) >> run
			where
				canSeeObj = canSeeObject (roomObjects ++ inventory)
				noVisObjMsg = ioOutMsgGS . notVisibleObjectError
				

main :: IO ()
main = do
	putStrLn $ lookAround startRoom startRoomObjects
	x <- evalStateT (runGameState run) initWorld
	putStrLn ""
		where
			startRoom = gsCurrentRoom $ initWorld
			startRoomObjects = locObjects . location $ startRoom