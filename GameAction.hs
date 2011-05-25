module GameAction where

import Types
import Locations
import Objects
import Text.Printf(printf)

------------------------------ Парсинг команды ---------------------------------
{-
			Walk Direction
			| Look
			| Examine ObjectName
			| Inventory
			| Take ObjectName
			| Weld ObjectName ObjectName
			| Open ObjectName
			| New
			| Quit String
			| Help
-}
type Parser a = [String] -> Maybe a

-- Параллельная композиция парсеров
(<<|>>) :: Parser a -> Parser a -> Parser a
p1 <<|>> p2 = \ss ->
	case p1 ss of
		Nothing -> p2 ss
		Just x -> Just x

cmdP :: (String, [String], ([String] -> Command)) -> Parser Command
cmdP (_, [], _) = \_ -> Nothing
cmdP (shortS, (cmdS:cmdSS), cmdConstr) = \(o:os) -> if (cmdS == o || shortS == o) && (length cmdSS <= length os)
												then Just $ cmdConstr cmdSS
												else Nothing

											
-- p :: (String, [String], ([String] -> Command))
lookP = ("L", ["Look"], \_ -> Look)
helpP = ("H", ["Help"], \_ -> Help)
openP = ("O", ["Open", "oName"], \(x:_) -> Open x)
examP = ("E", ["Examine", "oName"], \(x:_) -> Examine x)
invP  = ("I", ["Inventory"], \_ -> Inventory)
takeP = ("T", ["Take", "oName"], \(x:_) -> Take x)
weldP = ("W", ["Weld", "oName", "oName"], \(x:y:_) -> Weld x y)

parsers = map cmdP [lookP, helpP, openP]

{-

parse :: String -> Maybe Command
parse [] = Nothing
parse str = (foldr1 (<<|>>) parsers) (words str)-}
--------------------------------------------------------------------
initGameState :: GameState
initGameState = GameState {
	gsLocations = initialLocations,
	gsCurrentRoom = initialRoom,
	gsObjects = initialObjects
}

tryExamineObject :: Object -> GameAction
tryExamineObject obj = PrintMessage (objectDescription' obj)

tryWalk :: Location -> Direction -> GameState -> GameAction
tryWalk loc dir curGS@(GameState locs _ objects) =
		case walk loc dir locs of
			(Nothing, str) -> PrintMessage str
			(Just walkedLoc, str) -> SaveState newGS (str ++ "\n" ++ msg)
				where
					(msg, newWalkedLoc) = describeLocation walkedLoc (locationObjects walkedLoc objects)
					newLocs = updateLocations newWalkedLoc locs
					newGS = curGS {gsLocations = newLocs, gsCurrentRoom = locRoom newWalkedLoc}


tryTake :: Object -> GameState -> GameAction
tryTake obj curGS = let objects = gsObjects curGS
					 in case pickup obj of
						(Just newObj, msg) -> SaveState curGS {gsObjects = (replaceObject newObj objects)} msg
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
tryWeld obj1 obj2 curGS = case weld obj1 obj2 of
			Just (newObj, str) ->
				let (msg, newGS) = applyWeld obj1 obj2 newObj curGS in
				SaveState newGS (str ++ "\n" ++ msg)
			Nothing -> PrintMessage (failureWeldObjectsError obj1 obj2)

tryOpen :: Object -> GameState -> GameAction
tryOpen o gs@(GameState _ _ objects) = case open o of
											(Nothing, msg) -> PrintMessage msg
											(Just obj, msg)-> SaveState (gs {gsObjects = replaceObject obj objects}) msg
											
showInventory' :: GameState -> GameAction
showInventory' curGS = undefined

look' :: GameState -> GameAction
look' curGS = undefined

tryWalk' :: Direction -> GameState -> GameAction
tryWalk' dir curGS = undefined

tryTake' :: ObjectName -> GameState -> GameAction
tryTake' obj curGS = undefined

tryWeld' :: ObjectName -> ObjectName -> GameState -> GameAction
tryWeld' obj1 obj2 curGS = undefined

tryOpen' :: ObjectName -> GameState -> GameAction
tryOpen' obj curGS = undefined

tryExamineObject' :: ObjectName -> GameAction
tryExamineObject' obj = undefined