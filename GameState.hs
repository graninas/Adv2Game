module GameState where

import Types
import Locations
import Directions
import Objects

import Text.Printf(printf)


newGameState :: Locations -> Room -> LongDescribedRooms -> InventoryObjects -> GameState
newGameState newLocations newRoom newLongDescribedRooms newInventory = GameState {
	gsLocations = newLocations,
	gsCurrentRoom = newRoom,
	gsRoomLongDescribed = newLongDescribedRooms,
	gsInvObjects = newInventory
	}
	
--tryRiseObject :: Object -> (Maybe Object, String)
--tryRiseObject obj = if isPickupable obj then (Just obj, showObject obj ++ " added to your inventory.") else (Nothing, oPickupFailMsg obj)
--pickupObject :: ItemName -> GameState -> (String, GameState)
--pickupObject itmName curGS = (locDescription, newGameState (gsLocations curGS) room newLongDescribedRooms (gsInvObjects curGS))

tryInvestigateItem :: ItemName -> Objects -> Inventory -> GameAction
tryInvestigateItem itmName roomObjects inventory = undefined

walkTo :: Room -> GameState -> (String, GameState)
walkTo room curGS = (locDescription, curGS {gsCurrentRoom = room,
											gsRoomLongDescribed = newLongDescribedRooms}) --newGameState (gsLocations curGS) room newLongDescribedRooms (gsInvObjects curGS)
		where
			newLongDescribedRooms = if roomAlreadyLongDescribed then roomsDescribedEarlier else room : roomsDescribedEarlier
			roomAlreadyLongDescribed = isRoomLongDescribed roomsDescribedEarlier room
			roomsDescribedEarlier = gsRoomLongDescribed curGS
			locDescription = describeLocation roomAlreadyLongDescribed room (locationObjects (gsLocations curGS) room)
			
canWalk :: GameState -> Direction -> Maybe Room
canWalk = roomOnDirection . locPaths . location . gsCurrentRoom

tryWalk :: Direction -> GameState -> GameAction
tryWalk dir curGS = case canWalk curGS dir of
						Nothing -> PrintMessage (failureWalkingMsg dir)
						Just room -> SaveState newGS (successWalkingMsg room dir ++ "\n" ++ newLocDescr)
							where (newLocDescr, newGS) = walkTo room curGS

parseObject :: String -> Objects -> Maybe Object
parseObject _ [] = Nothing
parseObject str objects = case read str of
						[(x, "")] -> case isDigit x of
							True -> Just ( objects!!((digitToInt x)-1) )
							False -> Nothing
							
pickup :: Object -> GameState -> GameState
pickup = undefined

tryPickup' obj curGS = case isPickupable obj of
							False -> return (failurePickupingObjectMsg obj, Nothing, False)
							True -> return (successPickupingObjectMsg obj, Just (pickup obj curGS), False)		

tryPickup :: ItemName -> GameState -> GameAction
tryPickup itmName curGS = case canSeeObject (roomObjects ++ inventory) itmName of -- canSeeObject -> canSeeItem
			False -> return (notVisibleObjectError itmName, Nothing, False)
			True -> case exactlyObject of
				(Nothing, False, str) -> return (str, Nothing, False)
				(Just x, False, str) -> tryPickup' x curGS
				(_, True, str) -> return (str, Nothing, True)
			where
				currentRoom = gsCurrentRoom curGS
				roomObjects = locationObjects (gsLocations curGS) currentRoom
				inventory = gsInvObjects curGS
				matchedList = mathedObjects itmName roomObjects
				exactlyObject = case matchedList of
					[] -> (Nothing, False, "Error: no objects matched.")
					(x:[]) -> (Just x, False, successPickupingObjectMsg x)
					(xs) -> (Nothing, True, enumerateObjects "What object of these variants: " xs)