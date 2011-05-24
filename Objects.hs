module Objects where

import Types
import Tools
import Text.Printf(printf)
import qualified Data.List as L


--- Data functions ---
homePhone1 = Object "Digital Phone" NoRoom
homePhone2 = Object "Broken Phone" NoRoom
homeTable = Object "Table" NoRoom
homeUmbrella1 = Object "Red Umbrella" NoRoom
homeUmbrella2 = Object "Blue Umbrella" NoRoom
rope = Object "Rope" NoRoom
homeHook = Object "Hook" NoRoom
ropeOnHook = Complex "Rope on hook" rope homeHook NoRoom
homeLighter = Object "Lighter" NoRoom
homeDiary = Object "Diary" NoRoom
homeDrawer = Container "Drawer" Closed [homeDiary, homeLighter] NoRoom

objectDescription' :: Object -> String
objectDescription' obj  | obj == homeUmbrella1 = "Nice red mechanic Umbrella."
						| obj == homeUmbrella2 = "Nice blue Umbrella."
						| obj == homePhone1 = "The Phone has some voice messages for you."
						| obj == homePhone2 = "Broken electric phone."
						| obj == rope = "Good 30 meters rope."
						| obj == homeHook = "Massive steel hook nailed to wall."
						| obj == homeTable = "Good wooden table with drawer."
						| obj == homeDiary = "Your diary."
						| obj == ropeOnHook = "Rope on hook looks tight."
						| otherwise = printf "There is nothing special about %s." (showObject obj)

objectPickupFailMessage' :: Object -> String
objectPickupFailMessage' obj | obj == homePhone1 = "Phone drawes a wires and strikes against the table!"
							 | otherwise = printf "You can't take a %s." (showObject obj)

isPickupable :: Object -> Bool
isPickupable obj = obj `elem` [homeUmbrella1, rope]

isCombinable :: Object -> Object -> Bool
isCombinable obj1 obj2 | obj1 == rope && obj2 == homeHook = True
isCombinable obj1 obj2 | obj2 == rope && obj1 == homeHook = True

----------------------

isContainer :: Object -> Bool
isContainer (Container _ _ _ _) = True
isContainer _ = False

readObject :: String -> Objects -> Objects
readObject [] _ = []
readObject _ [] = []
readObject str (o:os) = let readObject' (x:xs) oNs = (any (== x) oNs) || readObject' xs oNs
						in case readObject' (words str) ((words . objectName) o) of
							True -> o : readObject str os
							False -> readObject str os

parseObject :: String -> Objects -> Either String Object
parseObject _ [] = Left "No objects to match."
parseObject [] _ = Left "What?"
parseObject str objects = case readObject str objects of
							[] -> case reads str :: [(Int, String)] of
								[(x,"")] -> case x >= 0 && x < (length objects) of
										True -> Right (objects !! x)
										False -> Left $ printf "Object with index %d does not exist." x
								_ -> Left $ printf "Can't parse an object '%s'." str
							(y:[]) -> Right y
							(ys) -> Left $ describeObjects "Ambiguous objects: " ys

-- Функция эквивалентности. Позволяет сравнивать объекты по их частичному совпадению.
(=|=) :: Object -> Object -> Bool
Complex _ x1 x2 _ =|= o@(Object _ _) = x1 == o || x2 == o
o@(Object _ _) =|= Complex _ y1 y2 _ = y1 == o || y2 == o
x =|= y = x == y

matchedObjects :: Object -> Objects -> Objects
matchedObjects _ [] = []
matchedObjects obj objects = filter (=|= obj) objects

replaceObject :: Object -> Objects -> Objects
replaceObject obj objects = obj : [newObj | newObj <- objects, newObj /= obj]
replaceObjectList :: Objects -> Objects -> Objects
replaceObjectList [] _ = []
replaceObjectList _ [] = []
replaceObjectList (n:ns) objects = replaceObjectList ns (replaceObject n objects)

pickup :: Object -> (Maybe Object, String)
pickup obj | objectRoom obj == InventoryRoom = (Nothing, objectAlreadyInInventoryError obj)
		   | otherwise = case isPickupable obj of
					True -> (Just (obj {objectRoom = InventoryRoom}), successPickupingObjectMsg obj)
					False -> (Nothing, failurePickupingObjectMsg obj)

wld1 (obj1:obj2:[]) | obj1 == rope && obj2 == homeHook = Just (ropeOnHook, "You successfully tied rope to the hook.")
wld1 _ = Nothing

-- Список комбинаторов объектов.
welders :: [Welder]
welders = [wld1]

-- w1 [1]  -?-> w1 [2] -?-> w1[3]  ...
-- Nothing ---> Just x ---> Just x ...
{-
(<||>) :: Welder -> Welder -> Welder
w1 <||> w2 = \os -> case w1 os of
						Just x -> Just x
						Nothing -> Nothing
-}

(<|>) :: Welder -> Welder -> Welder
w1 <|> w2 = \os -> let perm = L.permutations os
				   in case filter (/= Nothing) (map w1 perm) of
					[] -> case filter (/= Nothing) (map w2 perm) of
							[] -> Nothing
							(x:_) -> x
					(y:_) -> y
					
weld :: Object -> Object -> WeldedObject
weld o1 o2 = (foldr1 (<|>) welders) [o1, o2]

----------- Messages, Errors ------------

-- f :: Object -> String
notVisibleObjectError         obj = printf "You don't see any %s here." (showObject obj)
cannotBeOpenedError           obj = printf "The %s cannot be opened." (showObject obj)
cannotBeClosedError           obj = printf "The %s cannot be closed." (showObject obj)
alreadyOpenError              obj = printf "%s already opened." (showObject obj)
alreadyCloseError             obj = printf "%s already closed." (showObject obj)
successPickupingObjectMsg     obj = printf "%s added to your inventory." (showObject obj)
failurePickupingObjectMsg         = objectPickupFailMessage'
objectAlreadyInInventoryError obj = printf "You already have a %s." (showObject obj)

successOpeningObjectMsg :: Object -> Objects -> String
successOpeningObjectMsg obj [] = "Opened."
successOpeningObjectMsg obj (o:[]) = printf "Opening %s reveals %s." (showObject obj) (showObject o)
successOpeningObjectMsg obj os = describeObjects (printf "Opening %s reveals some objects: " (showObject obj)) os

instance Openable Object where
	open obj@(Container _ Opened _ _) = Left  $ alreadyOpenError obj
	open obj@(Container _ Closed _ _) = Right $ obj {objectContainerState = Opened}
	open obj = Left $ cannotBeOpenedError obj
	close obj@(Container _ Opened _ _) = Right $ obj {objectContainerState = Closed}
	close obj@(Container _ Closed _ _) = Left $ alreadyCloseError obj
	close obj = Left $ cannotBeClosedError obj
	showStated obj@(Container _ Opened _ _) = "(opened) " ++ showObject obj
	showStated obj = showObject obj
	showContents obj@(Container _ Opened cont@(x:xs) _) = describeObjects (printf "\nThe %s contains " (showObject obj)) cont
	showContents _ = []

----------------------- Функции отображения объекта и списка объектов. ---------------------------
type ObjectShowPrefix = (String, String)
type IntroString = String
type ShowObjectsFunc = ((Object -> Int -> String), (Int -> Int), Int)
type ShowObjectsBoundStrings = [String]

-- Вспомогательные функции
showLeftBracket :: ShowObjectsBoundStrings -> String
showRightBracket :: ShowObjectsBoundStrings -> String
showDelimiter :: ShowObjectsBoundStrings -> String
showLeftBracket = head
showRightBracket = head . tail
showDelimiter = last

standartObjectShowingF :: ShowObjectsFunc
standartObjectShowingF = ((\x _ -> showStated x), \_ -> 0, 0)

standartBoundStrs :: ShowObjectsBoundStrings
standartBoundStrs = ["[", "].", ", "]

-- Выводит информацию об объекте. Не перекрывает show, чтобы оставить возможность сохранять данные на диск.
showObject :: Object -> String
showObject = objectName

applyObjectShowingF :: ShowObjectsFunc -> Object -> String
applyObjectShowingF (showingLambda, _, enumVal) obj = showingLambda obj enumVal

modifyObjectShowingF :: ShowObjectsFunc -> ShowObjectsFunc
modifyObjectShowingF (showingLambda, enumChangeF, enumVal) = (showingLambda, enumChangeF, enumChangeF enumVal)

showObjects :: ObjectShowPrefix -> ShowObjectsFunc -> ShowObjectsBoundStrings -> Objects -> String
showObjects pref _          _         [] = fst pref
showObjects pref lFuncDescr boundStrs xs = snd pref ++ (showLeftBracket boundStrs) ++ showObjects' xs lFuncDescr
	where
		showObjects' (x:[]) lFuncDescr = applyObjectShowingF lFuncDescr x ++ (showRightBracket boundStrs)
		showObjects' (x:xs) lFuncDescr = applyObjectShowingF lFuncDescr x ++ (showDelimiter boundStrs) ++ showObjects' xs (modifyObjectShowingF lFuncDescr)

-- Перечисляет объекты в виде [списка]. Если не передана строка Intro, будет подставлена строка по умолчанию.
describeObjects :: IntroString -> Objects -> String
describeObjects [] os = (showObjects ([], "\nThere are some objects here: ") standartObjectShowingF standartBoundStrs os) ++ unwords(map showContents os)
describeObjects str os = (showObjects ([], str) standartObjectShowingF standartBoundStrs os) ++ unwords(map showContents os)

-- Показывает особые свойства объектов (если они есть).
investigateObjects :: IntroString -> Objects -> String
investigateObjects str = showObjects ([], str) ((\x _ -> printf "\n%s: %s" (showObject x) (objectDescription' x)), \_ -> 0, 0) ["","",""]

-- Перечисляет объекты инвентаря в виде [списка]. Если инвентарь пуст, так и сообщает.
showInventory :: Object -> String
showInventory (Container _ _ contents InventoryRoom) =  showObjects ("No objects in your inventory.", "You have: ") standartObjectShowingF standartBoundStrs contents
showInventory _ = []

-- Перечисляет объекты в виде пронумерованного списка, начинающегося с 0.
enumerateObjects :: IntroString -> Objects -> String
enumerateObjects str = showObjects ([], str) ((\x n -> printf "\n%d: %s" n (showObject x)), \y -> y + 1, 0) ["","",""]