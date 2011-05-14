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

readObject :: String -> Objects -> Objects
readObject [] _ = []
readObject _ [] = []
readObject s objects = filter (\x -> (capitalizedOName x) == capitalizedS) objects
	where
		capitalizedS = capitalize s
		capitalizedOName = capitalize . objectName

parseObject :: String -> Objects -> (Maybe Object, String)
parseObject _ [] = (Nothing, "No objects to match.")
parseObject [] _ = (Nothing, "What?")
parseObject str objects = case readObject str objects of
							[] -> case reads str :: [(Int, String)] of
								[(x,"")] -> case x >= 0 && x < (length objects) of
										True -> (Just (objects !! x), "")
										False -> (Nothing, printf "Object with index %d does not exist." x)
								_ -> (Nothing, printf "Can't parse an object '%s'." str)
							(y:[]) -> (Just y, "")
							(ys) -> (Nothing, describeObjects "Ambiguous objects: " ys)

type ObjectShowPrefix = (String, String)
type IntroString = String
type ShowObjectsFunc = ((Object -> Int -> String), (Int -> Int), Int)
type ShowObjectsBoundStrings = [String]

object :: ObjectIdentifier -> Object
object objID = Object {objectItem = snd objID, objectName = fst objID}

showObject :: Object -> String
showObject = objectName

notVisibleObjectError :: Item -> String
notVisibleObjectError item = printf "You don't see any %s here." (show item)

successPickupingObjectMsg :: Object -> String
successPickupingObjectMsg obj = showObject obj ++ " added to your inventory."

failurePickupingObjectMsg :: Object -> String
failurePickupingObjectMsg = objectPickupFailMessage' . objectName

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

describeObjects :: IntroString -> Objects -> String
describeObjects [] = showObjects ([], "\nThere are some objects here: ") standartObjectShowingF standartBoundStrs
describeObjects str = showObjects ([], str) standartObjectShowingF standartBoundStrs

investigateObjects :: IntroString -> Objects -> String
investigateObjects str = showObjects ([], str) ((\x _ -> printf "\n%s: %s" (showObject x) (objectDescription'. objectName $ x)), \_ -> 0, 0) ["","",""]

showInventory :: Inventory -> String
showInventory = showObjects ("No objects in your inventory.", "You have: ") standartObjectShowingF standartBoundStrs

enumerateObjects :: IntroString -> Objects -> String
enumerateObjects str = showObjects ([], str) ((\x n -> printf "\n%d: %s" n (showObject x)), \y -> y + 1, 0) ["","",""]

matchedObjects :: Item -> Objects -> Objects
matchedObjects _ [] = []
matchedObjects itm objects = filter (\x -> (objectItem x) == itm) objects

