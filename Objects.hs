module Objects where

import Types
import Text.Printf(printf)
import Items


--- Data functions ---

objectDescription' :: Item -> String
objectDescription' itm = case itm of
	homeUmbrella1 -> "Nice red mechanic Umbrella."
	homeUmbrella2 -> "Nice blue Umbrella."
	homePhone -> "The Phone has some voice messages for you."
	otherwise -> printf "There is nothing special about %s." (fst itm)

objectPickupFailMessage' :: Item -> String
objectPickupFailMessage' itm = case itm of
	homePhone -> "Phone drawes a wires and strikes against the table!"
	otherwise -> printf "You can't take a %s." (fst itm)

isPickupable :: Object -> Bool
isPickupable = flip elem [homeUmbrella1] . oItem

type ObjectShowPrefix = (String, String)

----------------------

object :: Item -> Object
object itm = Object {oItem = itm, oDescription = objectDescription' itm, oPickupFailMsg = objectPickupFailMessage' itm}

item :: Object -> Item
item obj = fst . oItem

notVisibleObjectError :: Object -> String
notVisibleObjectError obj = "You don't see any " ++ show obj ++ " here."

tryRiseObject :: Object -> (Maybe Object, String)
tryRiseObject obj = if isPickupable obj then (Just obj, show . item $ obj ++ " added to your inventory.") else (Nothing, oPickupFailMsg obj)

investigateObject :: Object -> String
investigateObject = oDescription

locationObjects :: Locations -> Room -> Objects
locationObjects [] _ = []
locationObjects (x:xs) room = if room == locRoom x then locObjects x else locationObjects xs room

canSeeObject :: Objects -> Object -> Bool
canSeeObject = flip elem

showObjects :: Objects -> ObjectShowPrefix -> String
showObjects [] pref = fst pref
showObjects xs pref = snd pref ++ showObjects' xs
	where
		showObjects' (x:[]) = show (item x)
		showObjects' (x:xs) = show (item x) ++ ", "
		showObjects' [] = "]."

describeObjects :: Objects -> String
describeObjects = showObjects ([], "There are some objects here: ")

showInventory :: Inventory -> String
showInventory = showObjects ("No objects in your inventory.", "You have: [")
