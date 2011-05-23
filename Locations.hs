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

locationShortDesc' :: Room -> String
locationShortDesc' room = case room of
	SouthRoom -> "You are standing in the middle room at the wooden table."
	NorthRoom -> "This is big light room."
	_ -> "Invalid room."
	
locationLongDesc' :: Room -> String
locationLongDesc' room = case room of
	SouthRoom -> "Room looks nice: small, clean, beauty. There is phone and papers on the big wooden table.  It is rainy and dark behind the window. A lightnings beat to the lighthouse on a mountain."
	NorthRoom -> "SouthRoom is the big nice place with many lamps on the walls."
	_ -> "Invalid room."
	
locationObjects' :: Room -> Objects
locationObjects' room = case room of
	SouthRoom -> objectDrawer : map object [homePhone, homeUmbrella1, homeTable, rope, homeHook, homeUmbrella2, homePhone2]
	NorthRoom -> map object [homeUmbrella2]
	_ -> []
	
initialLocations = M.fromList [
					(InventoryRoom, location InventoryRoom)
					(SouthRoom, location SouthRoom),
					(NorthRoom, location NorthRoom)]
initialLocation = 
	
----------------------
	
	
location :: Room -> Location
location room = Location {
							locRoom = room,
							locPaths = locationPaths' room,
							locObjects = locationObjects' room,
							locLongDescribed = False
							}

lookAround :: Location -> String
lookAround loc = locLongDescription loc ++ describeObjects [] (locObjects loc)

describeLocation :: Location -> Objects -> (String, Maybe Location)
describeLocation loc objects = case locLongDescribed loc of
								False -> (longDescr, Just (loc {locLongDescribed = True}))
								True -> (shortDescr, Nothing)
	where
		longDescr = (locLongDescription loc) ++ describeObjects [] objects
		shortDescr = (locShortDescription loc) ++ describeObjects [] objects
	
----------------------------------------------------------------------

getLocation :: Room -> Locations -> Maybe Location
getLocation room locs = case filter (\x -> locRoom x == room) locs of
					[] -> Nothing
					(x:xs) -> Just x

addObjectToLocation :: Location -> Object -> Location
addObjectToLocation loc obj = loc {locObjects = obj : (locObjects loc)}

removeObjectFromLocation :: Location -> Object -> Location
removeObjectFromLocation loc obj = loc {locObjects = [newObj | newObj <- locObjects loc, newObj /= obj]}

removeObjectListFromLocation :: Location -> Objects -> Location
removeObjectListFromLocation loc os = foldl removeObjectFromLocation loc os

updateLocations :: Location -> Locations -> Locations
updateLocations loc locs = loc : [newLoc | newLoc <- locs, (locRoom newLoc) /= (locRoom loc)]