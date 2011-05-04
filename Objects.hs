module Objects where

import Types
import Text.Printf(printf)
import Items


--- Data functions ---

objectDescription' :: Item -> String
objectDescription' itm
	| itm == homeUmbrella1 = "Nice red mechanic Umbrella."
	| itm == homeUmbrella2 = "Nice blue Umbrella."
	| itm == homePhone = "The Phone has some voice messages for you."
	| otherwise = printf "There is nothing special about %s." (show . fst $ itm)

objectPickupFailMessage' :: Item -> String
objectPickupFailMessage' itm
	| itm == homePhone = "Phone drawes a wires and strikes against the table!"
	| otherwise = printf "You can't take a %s." (show . fst $ itm)

isPickupable :: Object -> Bool
isPickupable = flip elem [homeUmbrella1] . oItem

type ObjectShowPrefix = (String, String)

----------------------

object :: Item -> Object
object itm = Object {oItem = itm, oDescription = objectDescription' itm, oPickupFailMsg = objectPickupFailMessage' itm}

itemName :: Object -> ItemName
itemName = fst . oItem

showObject :: Object -> String
showObject = show . itemName

thereAreObjects :: Objects -> ItemName -> Objects
thereAreObjects objects itemN = filter (\x -> (fst . oItem $ x) == itemN) objects

notVisibleObjectError :: ItemName -> String
notVisibleObjectError itmNm = printf "You don't see any %s here." (show itmNm)

tryRiseObject :: Object -> (Maybe Object, String)
tryRiseObject obj = if isPickupable obj then (Just obj, showObject obj ++ " added to your inventory.") else (Nothing, oPickupFailMsg obj)

investigateObject :: ItemName -> Objects -> String
investigateObject itemN objects = if not . null $ thereObjects then oDescription . head $ thereObjects else notVisibleObjectError itemN
	where thereObjects = thereAreObjects objects itemN

locationObjects :: Locations -> Room -> Objects
locationObjects [] _ = []
locationObjects (x:xs) room = if room == locRoom x then locObjects x else locationObjects xs room

canSeeObject :: Objects -> ItemName -> Bool
canSeeObject objects itemN = not . null $ thereObjects
	where thereObjects = thereAreObjects objects itemN

showObjects :: ObjectShowPrefix -> Objects -> String
showObjects pref [] = fst pref
showObjects pref xs = snd pref ++ showObjects' xs
	where
		showObjects' (x:[]) = showObject x
		showObjects' (x:xs) = showObject x ++ ", "
		showObjects' [] = "]."

describeObjects :: String -> Objects -> String
describeObjects [] = showObjects ([], "There are some objects here: ")
describeObjects str = showObjects ([], str)

showInventory :: InventoryObjects -> String
showInventory = showObjects ("No objects in your inventory.", "You have: [")

objectListFromObjectsByItemName :: ItemName -> Objects -> Objects
objectListFromObjectsByItemName _ [] = []
objectListFromObjectsByItemName itmNm objects = filter (\x -> (fst . oItem $ x) == itmNm) objects

