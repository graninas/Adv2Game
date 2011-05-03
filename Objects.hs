module Objects where

import Types

isPickupable :: Object -> Bool
isPickupable = flip elem [Umbrella]	-- По образу и подобию Advgame.

investigateObject :: Object -> String
investigateObject Umbrella = "Nice red mechanic umbrella."
investigateObject Phone = "The phone has some voice messages for you."
investigateObject obj = "There is nothing special about " ++ show obj ++ "."


notVisibleObjectError :: Object -> String
notVisibleObjectError obj = "You don't see any " ++ show obj ++ " here."

pickupFailMessage :: Object -> String
pickupFailMessage Phone = "Phone drawes a wires and strikes against the table!"
pickupFailMessage obj = "You can't take a " ++ show obj ++ "."

tryRiseObject :: Object -> (Maybe Object, String)
tryRiseObject obj = if isPickupable obj then (Just obj, show obj ++ " added to your inventory.") else (Nothing, pickupFailMessage obj)

locationObjects :: Locations -> Room -> Objects
locationObjects [] _ = []
locationObjects (x:xs) room = if room == locRoom x then locObjects x else locationObjects xs room

canSeeObject :: Objects -> Object -> Bool
canSeeObject objects = flip elem objects

showInventory :: Inventory -> String
showInventory [] = "No objects in your inventory."
showInventory inv = "You have: " ++ show inv

