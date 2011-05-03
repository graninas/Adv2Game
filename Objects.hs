module Objects where

import Types

isPickupable :: Object -> Bool
isPickupable = flip elem [Umbrella, Table]	-- По образу и подобию Advgame.

notVisibleObjectError :: Object -> String
notVisibleObjectError obj = "You don't see any " ++ show obj ++ " here."

pickupFailMessage :: Object -> String
pickupFailMessage Phone = "\nPhone drawes a wires and strikes against the table!"
pickupFailMessage obj = "\nYou can't take a " ++ show obj ++ "."

tryRiseObject :: Object -> (Maybe Object, String)
tryRiseObject obj = if isPickupable obj then (Just obj, show obj ++ " added to your inventory.") else (Nothing, pickupFailMessage obj)

locationObjects :: Locations -> Room -> Objects
locationObjects [] _ = []
locationObjects (x:xs) room = if room == locRoom x then locObjects x else locationObjects xs room

canSeeObject :: Object -> Objects -> Bool
canSeeObject object = elem object

showInventory :: Inventory -> String
showInventory [] = "No objects in your inventory."
showInventory inv = "You have: " ++ show inv