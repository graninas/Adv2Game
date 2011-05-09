module GameState where

import Types
import Locations
import Directions
import Objects

import Text.Printf(printf)
import Char(isDigit, digitToInt)

tryInvestigateItem :: ItemName -> Objects -> InventoryObjects -> GameAction
tryInvestigateItem itmName roomObjects inventory = undefined

walkTo :: Room -> GameState -> (String, GameState)
walkTo room curGS = (locDescription, curGS {gsCurrentRoom = room,
											gsRoomLongDescribed = newLongDescribedRooms})
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

tryTake :: String -> Objects -> GameState -> GameAction
tryTake str objects curGS = case parseObject str objects of
							(Just obj, _) -> tryPickup (fst $ oItem $ obj) [obj] curGS
							(Nothing, str) -> PrintMessage str
							
							
pickup :: Object -> GameState -> GameState
pickup obj curGS = curGS {gsLocations = (locationsWithoutObject locs room obj), gsInvObjects = obj : inv}
	where
		locs = gsLocations curGS
		room = gsCurrentRoom curGS
		inv = gsInvObjects curGS

tryPickup :: ItemName -> Objects -> GameState -> GameAction
tryPickup itmName fromObjects curGS = do
									let matched = matchedObjects itmName fromObjects
									case matched of
										[] -> PrintMessage (notVisibleObjectError itmName)
										(x:[]) -> case isPickupable x of
												True -> SaveState (pickup x curGS) (successPickupingObjectMsg x)
												False -> PrintMessage (failurePickupingObjectMsg x)
										(xs) -> ReadMessagedUserInput (enumerateObjects "What object of these variants: " xs) (QualifyPickup matched)