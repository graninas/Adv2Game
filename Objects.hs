module Objects where

import Types
import Items
import Tools
import Text.Printf(printf)


--- Data functions ---

objectName' :: Item -> String
objectName' itm
	| itm == homeUmbrella1 = "Red Umbrella"
	| itm == homeUmbrella2 = "Blue Umbrella"
	| itm == homePhone = "Digital Phone"
	| itm == homePhone2 = "Broken Phone"
	| otherwise = show . fst $ itm

objectDescription' :: Item -> String
objectDescription' itm
	| itm == homeUmbrella1 = "Nice red mechanic Umbrella."
	| itm == homeUmbrella2 = "Nice blue Umbrella."
	| itm == homePhone = "The Phone has some voice messages for you."
	| itm == homePhone2 = "Broken electric phone."
	| otherwise = printf "There is nothing special about %s." (show . fst $ itm)

objectPickupFailMessage' :: Item -> String
objectPickupFailMessage' itm
	| itm == homePhone = "Phone drawes a wires and strikes against the table!"
	| otherwise = printf "You can't take a %s." (show . fst $ itm)

isPickupable :: Object -> Bool
isPickupable = flip elem [homeUmbrella1] . oItem

----------------------

readsObject :: String -> Objects -> Objects
readsObject [] _ = []
readsObject _ [] = []
readsObject s objects = filter (\x -> (capitalizedOName x) == capitalizedS) objects
	where
		capitalizedS = capitalize s
		capitalizedOName = capitalize . oName

parseObject :: String -> Objects -> (Maybe Object, String)
parseObject _ [] = (Nothing, "No objects to match.")
parseObject str objects = case readsObject str objects of
							[] -> case reads str :: [(Int, String)] of
								[(x,"")] -> case x >= 0 && x < (length objects) of
										True -> (Just (objects !! x), "")
										False -> (Nothing, printf "Object with index %d does not exist." x)
								_ -> (Nothing, "Can't parse an object." ++ str)
							(y:[]) -> (Just y, "")
							(ys) -> (Nothing, describeObjects "Ambiguous objects: " ys)
						
	
type ObjectShowPrefix = (String, String)
type IntroString = String
type ShowObjectsFunc = ((Object -> Int -> String), (Int -> Int), Int)
type ShowObjectsBoundStrings = [String]

object :: Item -> Object
object itm = Object {oItem = itm, oName = objectName' itm, oDescription = objectDescription' itm, oPickupFailMsg = objectPickupFailMessage' itm}

itemName :: Object -> ItemName
itemName = fst . oItem

showObject :: Object -> String
showObject = oName

thereAreObjects :: Objects -> ItemName -> Objects
thereAreObjects objects itemN = filter (\x -> (fst . oItem $ x) == itemN) objects

notVisibleObjectError :: ItemName -> String
notVisibleObjectError itmNm = printf "You don't see any %s here." (show itmNm)

investigateObject :: ItemName -> Objects -> String
investigateObject itemN objects = if not . null $ thereObjects then oDescription . head $ thereObjects else notVisibleObjectError itemN
	where thereObjects = thereAreObjects objects itemN

locationObjects :: Locations -> Room -> Objects
locationObjects [] _ = []
locationObjects (x:xs) room = if room == locRoom x then locObjects x else locationObjects xs room

successPickupingObjectMsg :: Object -> String
successPickupingObjectMsg obj = showObject obj ++ " added to your inventory."

failurePickupingObjectMsg :: Object -> String
failurePickupingObjectMsg = oPickupFailMsg

canSeeItem :: Objects -> ItemName -> Bool
canSeeItem objects itemN = not . null $ thereObjects
	where thereObjects = thereAreObjects objects itemN

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

showInventory :: InventoryObjects -> String
showInventory = showObjects ("No objects in your inventory.", "You have: ") standartObjectShowingF standartBoundStrs

enumerateObjects :: IntroString -> Objects -> String
enumerateObjects str = showObjects ([], str) ((\x n -> printf "\n%d: " n ++ showObject x), \y -> y + 1, 1) ["","",""]

matchedObjects :: ItemName -> Objects -> Objects
matchedObjects _ [] = []
matchedObjects itmNm objects = filter (\x -> (fst . oItem $ x) == itmNm) objects

