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
							newOs <- replaceObject newObj objects
							SaveState curGS {gsObjects = newOs} msg
						(Nothing, msg) -> PrintMessage msg

-- "Применяет" результат команды Weld. Если новый объект можно взять, он добавляется в Инвентарь, если взять нельзя, остается в локации.
-- Два других объекта удаляются из локации.
applyWeld :: Object -> Object -> Object -> GameState -> (String, GameState)
applyWeld o1 o2 weldedO curGS =
		let
			curRoom = gsCurrentRoom curGS
			objects = gsObjects curGS
			(maybePickedUp, _) = pickup weldedO
			weldedInCurrentRoom = weldedO {objectRoom = curRoom}
			newO1 = o1 {objectRoom = NoRoom}
			newO2 = o2 {objectRoom = NoRoom}
			(msg, updatedObjects) =
				case maybePickedUp of
					Just newObj -> (printf "\n%s added to your Inventory." (showObject newObj),
									replaceObjectList [newObj, newO1, newO2] objects)
					Nothing -> ("", replaceObjectList [weldedInCurrentRoom, newO1, newO2] objects)
		in (msg, curGS {gsObjects = updatedObjects})

tryWeld :: Object -> Object -> GameState -> GameAction
tryWeld obj1 obj2 curGS = case weld


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