module Objects where

import Types

isPickupable :: Object -> Bool
isPickupable = flip elem [Umbrella]	-- По образу и подобию Advgame.

notVisibleObjectError :: Object -> String
notVisibleObjectError obj = "You don't see any " ++ show obj ++ " here."

pickupFailMessage :: Object -> String
pickupFailMessage x
	| x == Phone = "\nPhone drawes a wires and strikes against the table!"
	| otherwise = []

tryRiseObject :: Object -> (Maybe Object, String)
tryRiseObject obj = if isPickupable obj then (Just obj, "") else (Nothing, pickupFailMessage obj)

addToInventory :: Inventory -> Object -> Inventory
addToInventory inv obj = obj : inv

locationObjects :: Locations -> Room -> Objects
locationObjects [] _ = []
locationObjects (x:xs) room = if room == locRoom x then locObjects x else locationObjects xs room

canSeeObject :: Object -> Objects -> Bool
canSeeObject object = elem object

