module GameAction where

import Types
import Locations
import Objects
import Tools
import Text.Printf(printf)


------------------------------ Парсинг команды ---------------------------------
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
												then Just $ cmdConstr os
												else Nothing

-- p :: (String, [String], ([String] -> Command))
lookP = ("L", ["Look"], \_ -> Look)
helpP = ("H", ["Help"], \_ -> Help)
openP = ("O", ["Open", "oName"], \(x:_) -> Open x)
examP = ("E", ["Examine", "oName"], \(x:_) -> Examine x)
invP  = ("I", ["Inventory"], \_ -> Inventory)
takeP = ("T", ["Take", "oName"], \(x:_) -> Take x)
weldP = ("W", ["Weld", "oName", "oName"], \(x:y:_) -> Weld x y)
goP   = ("G", ["Go", "Direction"], \(x:_) -> Go x)
newP  = ([],  ["New"], \_ -> New)
quitP = ("Q", ["Quit"], \_ -> Quit "Be seen you...")

cmdParsers = map cmdP [lookP, helpP, openP, examP, invP, takeP, weldP, goP, newP, quitP]

parseCmd :: String -> Maybe Command
parseCmd [] = Nothing
parseCmd str = (foldr1 (<<|>>) cmdParsers) (map capitalize . words $ str)

---------------------------- Парсинг объекта -------------------------------
parseObject :: Room -> Objects -> ObjectName -> Either String Object
parseObject room os oName =
		case readObject oName (roomObjects room os) of
			[] -> Left $ printf "Can't parse an object %s." oName
			(x:[]) -> Right x
			xs -> Left $ enumerateObjects "What object of these variants: " xs

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

tryExamineObject :: Object -> GameAction
tryExamineObject obj = PrintMessage (objectDescription' obj)





showInventory' :: GameState -> GameAction
showInventory' (GameState _ _ objects) = PrintMessage $ showInventory $ roomObjects InventoryRoom objects

look' :: GameState -> GameAction
look' (GameState locs room objects) = case getLocation room locs of
			Just loc -> PrintMessage $ lookAround loc objects
			Nothing -> PrintMessage $ printf "Some error: no location on room %s was found." (show room)

tryWalk' :: Direction -> GameState -> GameAction
tryWalk' dir curGS = undefined

tryTake' :: ObjectName -> GameState -> GameAction
tryTake' objN gs@(GameState _ room objects) =
		case parseObject room objects objN of
			Left msg -> PrintMessage msg
			Right obj -> tryTake obj gs

tryWeld' :: ObjectName -> ObjectName -> GameState -> GameAction
tryWeld' obj1N obj2N gs@(GameState _ room objects) =
		case parseObject room objects obj1N of
			Left msg1 -> PrintMessage msg1
			Right obj1 -> case parseObject room objects obj2N of
							Left msg2 -> PrintMessage msg2
							Right obj2 -> tryWeld obj1 obj2 gs

tryOpen' :: ObjectName -> GameState -> GameAction
tryOpen' objN gs@(GameState _ room objects) =
		case parseObject room objects objN of
			Left _ -> case parseObject InventoryRoom objects objN of
				Left msg -> PrintMessage msg
				Right obj -> tryOpen obj gs
			Right obj -> tryOpen obj gs

tryExamineObject' :: ObjectName -> GameState -> GameAction
tryExamineObject' objN (GameState _ room objects) =
		case parseObject room objects objN of
			Left msg -> PrintMessage msg
			Right obj -> tryExamineObject obj
