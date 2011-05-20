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

tryExamineItem :: Item -> Objects -> GameAction
tryExamineItem itm fromObjects = let matched = matchedObjects itm fromObjects in
									case matched of
										[] -> PrintMessage (notVisibleObjectError itm)
										(x:[]) -> PrintMessage (objectDescription' . objectName $ x) -- Просто печатаем описание.
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


tryTakeS :: String -> Objects -> GameState -> GameAction
tryTakeS str objects curGS = case parseObject str objects of
							(Just obj, _) -> tryTake' obj curGS
							(Nothing, str) -> PrintMessage str

pickup :: Object -> Location -> Inventory -> (Maybe (Location, Inventory), String)
pickup obj loc inv = case isPickupable obj of
						True -> (Just (removeObjectFromLocation loc obj, obj : inv), successPickupingObjectMsg obj)
						False -> (Nothing, failurePickupingObjectMsg obj)

tryTake' :: Object -> GameState -> GameAction
tryTake' obj curGS = let
						loc = gsCurrentLocation curGS
						inv = gsInventory curGS
					in case pickup obj loc inv of
						(Just (newLoc, newInv), msg) -> SaveState curGS {gsCurrentLocation = newLoc, gsLocations = updateGsLocations newLoc curGS, gsInventory = newInv} msg
						(Nothing, msg) -> PrintMessage msg

tryTake :: Item -> Objects -> GameState -> GameAction
tryTake itm fromObjects curGS = let matched = matchedObjects itm fromObjects in
									case matched of
										[] -> PrintMessage (notVisibleObjectError itm)
										(x:[]) -> tryTake' x curGS
										(xs) -> ReadMessagedUserInput (enumerateObjects "What object of these variants: " xs) (QualifyPickup matched)

-- "Применяет" результат команды Weld. Если новый объект можно взять, он добавляется в Инвентарь, если взять нельзя, остается в локации.
-- Два других объекта удаляются из локации.
applyWeld :: Object -> Object -> Object -> GameState -> (String, GameState)
applyWeld o1 o2 weldedO curGS =
		let
			curLoc = gsCurrentLocation curGS
			inv = gsInventory curGS
			clearedLoc = removeObjectListFromLocation curLoc [o1, o2]
			maybeLocAndInv = pickup weldedO clearedLoc inv
			(msg, updatedLocs, updatedInv, updatedCurLoc) = case maybeLocAndInv of
				(Just (loc, newInv), _) -> ("\n" ++ showObject weldedO ++ " added to your Inventory.",
										updateGsLocations clearedLoc curGS,
										newInv,
										clearedLoc)
				(Nothing, _) -> ("", updateGsLocations (addObjectToLocation clearedLoc weldedO) curGS, inv, addObjectToLocation clearedLoc weldedO)
		in (msg, curGS {gsCurrentLocation = updatedCurLoc, gsLocations = updatedLocs, gsInventory = updatedInv})

tryWeld :: Item -> Item -> Objects -> GameState -> GameAction
tryWeld item1 item2 fromObjects curGS =
		let
			matched1 = matchedObjects item1 fromObjects
			matched2 = matchedObjects item2 fromObjects
			tooMany i os = PrintMessage (describeObjects (printf "Too many matches of %s: " (show i)) os)
		in
		case matched1 of
			[] -> PrintMessage $ notVisibleObjectError $ item1
			(x:[]) -> case matched2 of
					[] -> PrintMessage $ notVisibleObjectError $ item2
					(y:[]) -> case weld x y of
						(Just obj, str) ->
								let (applyMsg, newGS) = applyWeld x y obj curGS
								in SaveState newGS (str ++ applyMsg)
						(Nothing, str) ->  PrintMessage str
					(ys) -> tooMany item2 matched2
			(xs) -> tooMany item1 matched1

tryOpen' = undefined

tryOpen :: Item -> Objects -> GameState -> GameAction
tryOpen item fromObjects curGS =
		let matched = containerObjects (matchedObjects item fromObjects) in
			case matched of
				[] -> PrintMessage (notVisibleObjectError item)
				(x:[]) -> tryOpen' x curGS
				(xs) -> ReadMessagedUserInput (enumerateObjects "What object you want to open: " xs) (QualifyPickup matched)