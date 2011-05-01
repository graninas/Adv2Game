module Objects where

import Types

isPickupable :: Object -> Bool
isPickupable = flip elem [Umbrella]	-- По образу и подобию Advgame.

pickupFailMessage :: Object -> String
pickupFailMessage x
	| x == Phone = "\nPhone drawes a wires and strikes against the table!"
	| otherwise = []

tryRiseObject :: Object -> (Maybe Object, String)
tryRiseObject obj = if isPickupable obj then (Just obj, "") else (Nothing, pickupFailMessage obj)



locationObjects :: Locations -> Room -> Objects
locationObjects [] _ = []
locationObjects (x:xs) room = if room == locRoom x then locObjects x else locationObjects xs room