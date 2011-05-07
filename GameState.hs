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
	
successWalkingMsg :: Room -> Direction -> GameState -> String
successWalkingMsg _ dir _ = printf "You walking to %s."  (show dir)

walkTo :: Room -> GameState -> (String, GameState)
walkTo room curGS = (locDescription, newGameState (gsLocations curGS) room newLongDescribedRooms (gsInvObjects curGS))
		where
			newLongDescribedRooms = if roomAlreadyLongDescribed then roomsDescribedEarlier else room : roomsDescribedEarlier
			roomAlreadyLongDescribed = isRoomLongDescribed roomsDescribedEarlier room
			roomsDescribedEarlier = gsRoomLongDescribed curGS
			locDescription = describeLocation roomAlreadyLongDescribed room (locationObjects (gsLocations curGS) room)

canWalk :: GameState -> Direction -> Maybe Room
canWalk = roomOnDirection . locPaths . location . gsCurrentRoom

tryWalk :: Direction -> GameState -> GS (String, Maybe GameState)
tryWalk dir curGS = case canWalk curGS dir of
						Just room -> return (successWalkingMsg room dir curGS ++ "\n" ++ newLocDescr, Just newGS)
							where (newLocDescr, newGS) = walkTo room curGS
						Nothing -> return (failureWalkingMsg dir, Nothing)