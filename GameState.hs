module GameState where

import Types
import Locations
import Paths
import Objects

import Text.Printf(printf)

initGameState :: GameState
initGameState = GameState {
	gsLocations = initialLocations,
	gsCurrentRoom = initialRoom,
	gsObjects = initialObjects
}

updateGsLocations :: Location -> GameState -> Locations
updateGsLocations newLoc (GameState locs _ _) = updateLocations newLoc locs

tryExamineObject :: Object -> GameAction
tryExamineObject obj = PrintMessage (objectDescription' obj)

tryWalk' :: Location -> Direction -> Locations -> Maybe Room
tryWalk' (Location _ paths _ _ _ _) toDir locs = getPathOnDirection paths toDir >>= pathRoom

tryWalk :: Location -> Direction -> GameState -> GameAction
tryWalk fromLoc toDir curGS@(GameState locs _ objects) = case tryWalk' fromLoc toDir locs of
						Nothing -> PrintMessage (failureWalkingMsg toDir)
						Just room -> SaveState newGS (successWalkingMsg room toDir ++ "\n" ++ newLocDescr)
							where
								walkedLoc = getLocation room locs
								(newLocDescr, maybeUpdatedLoc) = describeLocation walkedLoc objects
								newGS = curGS {gsCurrentRoom = room, gsLocations = updatedLocs}
								newLoc = walkedLoc {locLongDescribed = True}
								updatedLocs = updateGsLocations newLoc curGS


tryTakeS :: String -> Objects -> GameState -> GameAction
tryTakeS str objects curGS = case parseObject str objects of
							Right obj -> tryTake obj curGS
							Left str -> PrintMessage str

tryTake :: Object -> GameState -> GameAction
tryTake obj curGS = let objects = gsObjects curGS
					 in case pickup obj of
						(Just newObj, msg) -> do
							newOs <- updateObjects newObj objects
							SaveState curGS {gsObjects = newOs} msg
						(Nothing, msg) -> PrintMessage msg

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
										newInv, clearedLoc)
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


applyOpenedObject newObj loc locs inv = let
								newLoc = loc {locObjects = updateObjects newObj (locObjects loc)} -- Подменяем один объект из списка объектов новым объектом.
								newLocs = updateLocations newLoc locs -- Подменяем одну локацию из списка локаций новой локацией.
								newInv = updateObjects newObj inv
							 in (newLoc, newLocs, newInv)

tryOpenS :: String -> Objects -> GameState -> GameAction
tryOpenS str objects curGS = case parseObject str objects of
							Right obj -> tryOpen' obj curGS
							Left str -> PrintMessage str

tryOpen' :: Object -> GameState -> GameAction
tryOpen' o gs@(GameState locs curLoc inv) =
	case open o of
		Left errorMsg -> PrintMessage errorMsg
		Right newO -> let
						(newLoc, newLocs, newInv) = applyOpenedObject newO curLoc locs inv
						newState = gs {gsLocations = newLocs, gsCurrentLocation = newLoc, gsInventory = newInv}
						msg = successOpeningObjectMsg newO (objectContents newO)
					  in SaveState newState msg

tryOpen :: Item -> Objects -> Inventory -> GameState -> GameAction
tryOpen item locObjects inv curGS = let matched = matchedObjects item (locObjects ++ inv) in
		case matched of
			[] -> PrintMessage (notVisibleObjectError item)
			(x:[]) -> tryOpen' x curGS
			(xs) -> ReadMessagedUserInput (enumerateObjects "What object you want to open: " xs) (QualifyOpen matched)