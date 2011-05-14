module GameState where

import Types
import Locations
import Paths
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

tryWalk' :: Location -> Direction -> Locations -> Maybe Location
tryWalk' fromLoc toDir locs = getPathOnDirection (locPaths fromLoc) toDir >>=  \x -> getLocation (pathRoom x) locs

tryWalk :: Location -> Direction -> GameState -> GameAction
tryWalk fromLoc toDir curGS = case tryWalk' fromLoc toDir (gsLocations curGS) of
						Nothing -> PrintMessage (failureWalkingMsg toDir)
						Just walkedLoc -> SaveState newGS (successWalkingMsg (locRoom walkedLoc) toDir ++ "\n" ++ newLocDescr)
							where
								(newLocDescr, maybeUpdatedLoc) = describeLocation walkedLoc (locObjects walkedLoc)
								newGS = curGS {gsCurrentLocation = newLoc, gsLocations = updatedLocs}
								newLoc = walkedLoc {locLongDescribed = True}
								updatedLocs = updateGsLocations newLoc curGS

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

-- "Применяет" результат команды Weld. Если новый объект можно взять, он добавляется в Инвентарь, если взять нельзя, остается в локации.
-- Два других объекта удаляются из локации.
applyWeld :: Object -> Object -> Object -> GameState -> (String, GameState)
applyWeld o1 o2 weldedO curGS =
		let
			curLoc = gsCurrentLocation curGS
			inv = gsInventory curGS
			clearedLoc = removeObjectListFromLocation curLoc [o1, o2]
			maybeLocAndInv = tryPickup' weldedO clearedLoc inv
			(msg, updatedLocs, updatedInv, updatedCurLoc) = case maybeLocAndInv of
				Just (loc, newInv) -> ("\n" ++ showObject weldedO ++ " added to your Inventory.",
										updateGsLocations clearedLoc curGS,
										newInv,
										clearedLoc)
				Nothing -> ("", updateGsLocations (addObjectToLocation clearedLoc weldedO) curGS, inv, addObjectToLocation clearedLoc weldedO)
		in (msg, curGS {gsCurrentLocation = updatedCurLoc, gsLocations = updatedLocs, gsInventory = updatedInv})

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
						(Just obj, str) ->
								let (applyMsg, newGS) = applyWeld x y obj curGS
								in SaveState newGS (str ++ applyMsg)
						(Nothing, str) ->  PrintMessage str
					(ys) -> tooMany itmName2 matched2
			(xs) -> tooMany itmName1 matched1
