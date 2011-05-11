module GameState where

import Types
import Locations
import Directions
import Objects

import Text.Printf(printf)
import Char(isDigit, digitToInt)

initGameState :: GameState
initGameState = GameState {
	gsLocations = [location SouthRoom, location NorthRoom],
	gsCurrentLocation = location SouthRoom,
	gsInventory = []
}

updateGsLocations :: Location -> GameState -> Locations
updateGsLocations newLoc curGS = updateLocations newLoc (gsLocations curGS)

tryInvestigateItem :: ItemName -> Objects -> GameAction
tryInvestigateItem itmName fromObjects = let matched = matchedObjects itmName fromObjects in
									case matched of
										[] -> PrintMessage (notVisibleObjectError itmName)
										(x:[]) -> PrintMessage (oDescription x)
										(xs) -> PrintMessage (investigateObjects "You look fixedly at objects." matched)

walkTo :: Room -> GameState -> (String, GameState)
walkTo room curGS = (str, curGS {gsCurrentLocation = locWalkTo, gsLocations = updatedLocs})
		where
			locWalkTo = getLocation room (gsLocations curGS)
			(str, maybeNewLoc) = describeLocation locWalkTo (locObjects locWalkTo)
			updatedLocs = case maybeNewLoc of
						Just updatedLoc -> updateGsLocations updatedLoc curGS
						Nothing -> (gsLocations curGS)
			

canWalk :: GameState -> Direction -> Maybe Room
canWalk = roomOnDirection . locPaths . gsCurrentLocation

tryWalk :: Direction -> GameState -> GameAction
tryWalk dir curGS = case canWalk curGS dir of
						Nothing -> PrintMessage (failureWalkingMsg dir)
						Just room -> SaveState newGS (successWalkingMsg room dir ++ "\n" ++ newLocDescr)
							where (newLocDescr, newGS) = walkTo room curGS

tryTake :: String -> Objects -> GameState -> GameAction
tryTake str objects curGS = case parseObject str objects of
							(Just obj, _) -> tryPickup (fst $ oItem $ obj) [obj] curGS
							(Nothing, str) -> PrintMessage str
							
							
tryPickup' :: Object -> Location -> Inventory -> Maybe (Location, Inventory)
tryPickup' obj loc inv = case isPickupable obj of
						True -> Just (removeObjectFromLocation loc obj, obj : inv)
						False -> Nothing

tryPickup :: ItemName -> Objects -> GameState -> GameAction
tryPickup itmName fromObjects curGS = let matched = matchedObjects itmName fromObjects in
									case matched of
										[] -> PrintMessage (notVisibleObjectError itmName)
										(x:[]) -> case tryPickup' x (gsCurrentLocation curGS) (gsInventory curGS) of
												Just (newLoc, newInv) -> SaveState curGS {gsCurrentLocation = newLoc, gsLocations = updateGsLocations newLoc curGS, gsInventory = newInv} (successPickupingObjectMsg x)
												Nothing -> PrintMessage (failurePickupingObjectMsg x)
										(xs) -> ReadMessagedUserInput (enumerateObjects "What object of these variants: " xs) (QualifyPickup matched)

applyWeld :: Object -> Object -> Object -> GameState -> GameState
applyWeld o1 o2 weldedO curGS = curGS {gsLocations = newLocations, gsInventory = newInventory}
	where
		curLoc = gsCurrentLocation curGS
		inv = gsInventory curGS
		clearedLocation = removeObjectListFromLocation curLoc [o1, o2]
		pickupTrying = tryPickup' weldedO curLoc inv
		newLocations = case pickupTrying of
				Just (newLoc, newInv) -> updateGsLocations clearedLocation curGS
				Nothing -> updateGsLocations (addObjectToLocation clearedLocation weldedO) curGS
		newInventory = case pickupTrying of
				Just (_, newInv) -> newInv
				Nothing -> inv

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
