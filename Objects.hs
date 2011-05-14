module Objects where

import Types
import Tools
import Text.Printf(printf)


--- Data functions ---

homePhone = ("Digital Phone", Phone)
homePhone2 = ("Broken Phone", Phone)
homeTable = ("", Table)
homeUmbrella1 = ("Red Umbrella", Umbrella)
homeUmbrella2 = ("Blue Umbrella", Umbrella)
homeDrawer = ("", Drawer)
rope = ("", Rope)
homeHook = ("", Hook)
ropeOnHook = ("Rope on hook", TiedRope)

objectName' :: ObjectID -> String
objectName' ([], itmName) = show itmName
objectName' (n, _) = n

objectDescription' :: ObjectID -> String
objectDescription' objID
	| objID == homeUmbrella1 = "Nice red mechanic Umbrella."
	| objID == homeUmbrella2 = "Nice blue Umbrella."
	| objID == homePhone = "The Phone has some voice messages for you."
	| objID == homePhone2 = "Broken electric phone."
	| objID == rope = "Good 30 meters rope."
	| objID == homeHook = "Massive steel hook nailed to wall."
	| objID == ropeOnHook = "Rope on hook looks tight."
	| otherwise = printf "There is nothing special about %s." (objectName' objID)

objectPickupFailMessage' :: ObjectID -> String
objectPickupFailMessage' objID
	| objID == homePhone = "Phone drawes a wires and strikes against the table!"
	| otherwise = printf "You can't take a %s." (objectName' objID)

isPickupable :: Object -> Bool
isPickupable obj = (objectID obj) `elem` [homeUmbrella1, rope]

weld :: Object -> Object -> (Maybe Object, String)
weld o1 o2
	| objectID o1 == rope && objectID o2 == homeHook = (Just $ object ropeOnHook, "You successfully tied rope to the hook.")
	| otherwise = (Nothing, printf "You can't weld %s to %s." (showObject o1) (showObject o2))

----------------------

readObject :: String -> Objects -> Objects
readObject [] _ = []
readObject _ [] = []
readObject s objects = filter (\x -> (capitalizedOName x) == capitalizedS) objects
	where
		capitalizedS = capitalize s
		capitalizedOName = capitalize . fst . objectID

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

object :: ObjectID -> Object
object objID = Object {objectID = objID}

showObject :: Object -> String
showObject = objectName' . objectID

notVisibleObjectError :: ItemName -> String
notVisibleObjectError itmNm = printf "You don't see any %s here." (show itmNm)

successPickupingObjectMsg :: Object -> String
successPickupingObjectMsg obj = showObject obj ++ " added to your inventory."

failurePickupingObjectMsg :: Object -> String
failurePickupingObjectMsg = objectPickupFailMessage' . objectID

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
investigateObjects str = showObjects ([], str) ((\x _ -> printf "\n%s: %s" (showObject x) (objectDescription'. objectID $ x)), \_ -> 0, 0) ["","",""]

showInventory :: Inventory -> String
showInventory = showObjects ("No objects in your inventory.", "You have: ") standartObjectShowingF standartBoundStrs

enumerateObjects :: IntroString -> Objects -> String
enumerateObjects str = showObjects ([], str) ((\x n -> printf "\n%d: %s" n (showObject x)), \y -> y + 1, 0) ["","",""]

matchedObjects :: ItemName -> Objects -> Objects
matchedObjects _ [] = []
matchedObjects itmName objects = filter (\x -> (snd . objectID $ x) == itmName) objects

