module Locations where

import Types
import Objects
import qualified Data.Map as M

--- Data functions ---

locationPaths' :: Room -> Paths
locationPaths' room = case room of
	SouthRoom -> [Path North NorthRoom, Path South SouthRoom]
	NorthRoom -> [Path South SouthRoom, Path West Corridor]
	_ -> []

locationShortDescription' :: Location -> String
locationShortDescription' (Location room _ _) = case room of
	SouthRoom -> "You are standing in the middle room at the wooden table."
	NorthRoom -> "This is big light room."
	_ -> "Invalid room."

locationLongDescription' :: Location -> String
locationLongDescription' (Location room _ _) = case room of
	SouthRoom -> "Room looks nice: small, clean, beauty. There is phone and papers on the big wooden table.  It is rainy and dark behind the window. A lightnings beat to the lighthouse on a mountain."
	NorthRoom -> "SouthRoom is the big nice place with many lamps on the walls."
	_ -> "Invalid room."
	
initialObjects :: Objects
initialObjects = map (\x -> x {objectRoom = SouthRoom}) [homeDrawer, homePhone1, homeUmbrella1, homeTable, rope, homeHook, homeUmbrella2] ++
				 map (\x -> x {objectRoom = NorthRoom}) [homePhone2]
	
initialLocations = M.fromList [
					(InventoryRoom, location InventoryRoom),
					(SouthRoom, location SouthRoom),
					(NorthRoom, location NorthRoom)]

initialRoom = SouthRoom

----------------------
instance Eq Location where
	loc1 == loc2 = locRoom loc1 == locRoom loc2


locationObjects :: Location -> Objects -> Objects
locationObjects loc = filter (\x -> locRoom loc == objectRoom x)

location :: Room -> Location
location room = Location {
							locRoom = room,
							locPaths = locationPaths' room,
							locLongDescribed = False
							}

lookAround :: Location -> Objects -> String
lookAround loc os = locationLongDescription' loc ++ describeObjects [] (locationObjects loc os)

describeLocation :: Location -> Objects -> (String, Maybe Location)
describeLocation loc objects = case locLongDescribed loc of
								False -> (longDescr, Just (loc {locLongDescribed = True}))
								True -> (shortDescr, Nothing)
	where
		longDescr = (locationLongDescription' loc) ++ describeObjects [] (locationObjects loc objects)
		shortDescr = (locationShortDescription' loc) ++ describeObjects [] (locationObjects loc objects)
	
----------------------------------------------------------------------

getLocation :: Room -> Locations -> Maybe Location
getLocation = M.lookup

setObjectLocation :: Object -> Location -> Object
setObjectLocation obj loc = obj {objectRoom = locRoom loc}

updateLocations :: Location -> Locations -> Locations
updateLocations loc = M.update (\x -> if x == loc then Just loc else Nothing) (locRoom loc)