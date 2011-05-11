module GameState where

import Types
import Locations
import Directions
import Objects

import Text.Printf(printf)
import Char(isDigit, digitToInt)

tryInvestigateItem :: ItemName -> Objects -> GameAction
tryInvestigateItem itmName fromObjects = let matched = matchedObjects itmName fromObjects in
									case matched of
										[] -> PrintMessage (notVisibleObjectError itmName)
										(x:[]) -> PrintMessage (oDescription x)
										(xs) -> PrintMessage (investigateObjects "You look fixedly at objects." matched)

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
							
							
tryPickup' :: Object -> GameState -> Maybe GameState
tryPickup' obj curGS = let
						locs = gsLocations curGS
						room = gsCurrentRoom curGS
						inv = gsInvObjects curGS in
					case isPickupable obj of
						True -> Just curGS {gsLocations = (locationsWithoutObject locs room obj), gsInvObjects = obj : inv}
						False -> Nothing
		

tryPickup :: ItemName -> Objects -> GameState -> GameAction
tryPickup itmName fromObjects curGS = let matched = matchedObjects itmName fromObjects in
									case matched of
										[] -> PrintMessage (notVisibleObjectError itmName)
										(x:[]) -> case tryPickup' x curGS of
												Just newState -> SaveState newState (successPickupingObjectMsg x)
												Nothing -> PrintMessage (failurePickupingObjectMsg x)
										(xs) -> ReadMessagedUserInput (enumerateObjects "What object of these variants: " xs) (QualifyPickup matched)

applyWeld :: Object -> Object -> Object -> GameState -> GameState
applyWeld o1 o2 weldedO curGS = case tryPickup' weldedO curGS of
									Just newState -> newState {gsLocations = locsWithoutObjects}
									Nothing -> curGS {gsLocations = locsPlusWeldedObject}
	where
		locs = gsLocations curGS
		room = gsCurrentRoom curGS
		inv = gsInvObjects curGS
		locsWithoutObjects = locationsWithoutObjects room locs [o1,o2]
		locsPlusWeldedObject = addObjectToLocation locsWithoutObjects (gsCurrentRoom curGS) weldedO

tryWeld :: ItemName -> ItemName -> Objects -> GameState -> GameAction
tryWeld itmName1 itmName2 fromObjects curGS =
		let
			matched1 = matchedObjects itmName1 fromObjects
			matched2 = matchedObjects itmName2 fromObjects
			tooMany i os = PrintMessage (describeObjects (printf "Too many matches of %s: " (show i)) os)
		in
		case matched1 of
			[] -> PrintMessage $ notVisibleObjectError $ itmName1
			(x:[]) -> case matched2 of
					[] -> PrintMessage $ notVisibleObjectError $ itmName2
					(y:[]) -> case weld x y of
						(Just obj, str) -> SaveState (applyWeld x y obj curGS) str
						(Nothing, str) ->  PrintMessage str
					(ys) -> tooMany itmName2 matched2
			(xs) -> tooMany itmName1 matched1
