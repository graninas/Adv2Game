module Objects where

import Types
import Tools
import Text.Printf(printf)


--- Data functions ---

homePhone = ("Digital Phone", Phone)
homePhone2 = ("Broken Phone", Phone)
homeTable = ("Table", Table)
homeUmbrella1 = ("Red Umbrella", Umbrella)
homeUmbrella2 = ("Blue Umbrella", Umbrella)
homeDrawer = ("Drawer", Drawer)
rope = ("Rope", Rope)
homeHook = ("Hook", Hook)
ropeOnHook = ("Rope on hook", Combined Rope Hook)
homeLighter = ("Lighter", Lighter)

objectLighter = Object (snd homeLighter) (fst homeLighter) NotContainer []
objectDrawer = Object (snd homeDrawer) (fst homeDrawer) Closed [objectLighter]

objectDescription' :: ObjectName -> String
objectDescription' objName
	| objName == fst homeUmbrella1 = "Nice red mechanic Umbrella."
	| objName == fst homeUmbrella2 = "Nice blue Umbrella."
	| objName == fst homePhone = "The Phone has some voice messages for you."
	| objName == fst homePhone2 = "Broken electric phone."
	| objName == fst rope = "Good 30 meters rope."
	| objName == fst homeHook = "Massive steel hook nailed to wall."
	| objName == fst ropeOnHook = "Rope on hook looks tight."
	| otherwise = printf "There is nothing special about %s." objName

objectPickupFailMessage' :: ObjectName -> String
objectPickupFailMessage' objName
	| objName == fst homePhone = "Phone drawes a wires and strikes against the table!"
	| otherwise = printf "You can't take a %s." objName

isPickupable :: Object -> Bool
isPickupable obj = (objectName obj, objectItem obj) `elem` [homeUmbrella1, rope]

weld :: Object -> Object -> (Maybe Object, String)
weld o1 o2
	| objectName o1 == fst rope && objectName o2 == fst homeHook = (Just $ object ropeOnHook, "You successfully tied rope to the hook.")
	| otherwise = (Nothing, printf "You can't weld %s to %s." (showObject o1) (showObject o2))

----------------------

object :: ObjectIdentifier -> Object
object objID = Object (snd objID) (fst objID) NotContainer []

isContainer :: Object -> Bool
isContainer (Object _ _ contState _)
	| contState /= NotContainer =  True
	| otherwise = False

readObject :: String -> Objects -> Objects
readObject [] _ = []
readObject _ [] = []
readObject s objects = filter (\x -> (capitalizedOName x) == capitalizedS) objects
	where
		capitalizedS = capitalize s
		capitalizedOName = capitalize . objectName

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

type ObjectShowPrefix = (String, String)
type IntroString = String
type ShowObjectsFunc = ((Object -> Int -> String), (Int -> Int), Int)
type ShowObjectsBoundStrings = [String]

-- Выводит информацию об объекте. Не перекрывает show, чтобы оставить возможность сохранять данные на диск.
showObject :: Object -> String
showObject = objectName

showObjects :: ObjectShowPrefix -> ShowObjectsFunc -> ShowObjectsBoundStrings -> Objects -> String
showObjects pref _          _         [] = fst pref
showObjects pref lFuncDescr boundStrs xs = snd pref ++ (showLeftBracket boundStrs) ++ showObjects' xs lFuncDescr
	where
		showObjects' (x:[]) lFuncDescr = applyObjectShowingF lFuncDescr x ++ (showRightBracket boundStrs)
		showObjects' (x:xs) lFuncDescr = applyObjectShowingF lFuncDescr x ++ (showDelimiter boundStrs) ++ showObjects' xs (modifyObjectShowingFunc lFuncDescr)

showLeftBracket :: ShowObjectsBoundStrings -> String
showRightBracket :: ShowObjectsBoundStrings -> String
showDelimiter :: ShowObjectsBoundStrings -> String
showLeftBracket = head
showRightBracket = head . tail
showDelimiter = last

standartObjectShowingF :: ShowObjectsFunc
standartObjectShowingF = ((\x _ -> showObject x), \_ -> 0, 0)

standartBoundStrs :: ShowObjectsBoundStrings
standartBoundStrs = ["[", "].", ", "]

modifyObjectShowingFunc :: ShowObjectsFunc -> ShowObjectsFunc
modifyObjectShowingFunc (showingLambda, enumChangeF, enumVal) = (showingLambda, enumChangeF, enumChangeF enumVal)

applyObjectShowingF :: ShowObjectsFunc -> Object -> String
applyObjectShowingF (showingLambda, _, enumVal) obj = showingLambda obj enumVal

-- Перечисляет объекты в виде [списка]. Если не передана строка Intro, будет подставлена строка по умолчанию.
describeObjects :: IntroString -> Objects -> String
describeObjects [] = showObjects ([], "\nThere are some objects here: ") standartObjectShowingF standartBoundStrs
describeObjects str = showObjects ([], str) standartObjectShowingF standartBoundStrs

-- Показывает особые свойства объектов (если они есть).
investigateObjects :: IntroString -> Objects -> String
investigateObjects str = showObjects ([], str) ((\x _ -> printf "\n%s: %s" (showObject x) (objectDescription'. objectName $ x)), \_ -> 0, 0) ["","",""]

-- Перечисляет объекты инвентаря в виде [списка]. Если инвентарь пуст, так и сообщает.
showInventory :: Inventory -> String
showInventory = showObjects ("No objects in your inventory.", "You have: ") standartObjectShowingF standartBoundStrs

-- Перечисляет объекты в виде пронумерованного списка, начинающегося с 0.
enumerateObjects :: IntroString -> Objects -> String
enumerateObjects str = showObjects ([], str) ((\x n -> printf "\n%d: %s" n (showObject x)), \y -> y + 1, 0) ["","",""]

isItemsEqual :: Item -> Item -> Bool
Combined x1 x2 `isItemsEqual` y = x1 == y || x2 == y
x `isItemsEqual` Combined y1 y2 = y1 == x || y2 == x
x `isItemsEqual` y = x == y

matchedObjects :: Item -> Objects -> Objects
matchedObjects _ [] = []
matchedObjects itm objects = filter (\x -> isItemsEqual (objectItem x) itm) objects

updateObjects :: Object -> Objects -> Objects
updateObjects obj objects = obj : [newObj | newObj <- objects, (objectName newObj) /= (objectName obj)]

----------- Messages, Errors ------------

notVisibleObjectError :: Item -> String
notVisibleObjectError item = printf "You don't see any %s here." (show item)

cannotBeOpenedError :: Object -> String
cannotBeOpenedError obj = printf "The %s cannot be opened." (showObject obj)

alreadyOpenedError :: Object -> String
alreadyOpenedError obj = printf "%s already open." (showObject obj)

successPickupingObjectMsg :: Object -> String
successPickupingObjectMsg obj = showObject obj ++ " added to your inventory."

failurePickupingObjectMsg :: Object -> String
failurePickupingObjectMsg = objectPickupFailMessage' . objectName

successOpeningObjectMsg :: Object -> Objects -> String
successOpeningObjectMsg obj [] = "Opened."
successOpeningObjectMsg obj (o:[]) = printf "Opening %s reveals %s." (showObject obj) (showObject o)
successOpeningObjectMsg obj os = describeObjects (printf "Opening %s reveals some objects: ") os
