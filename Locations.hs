module Locations where

import Types
import Items

--- Data functions ---

locationPaths' :: Room -> Paths
locationPaths' room = case room of
	SouthRoom -> [Path North NorthRoom, Path South SouthRoom]
	NorthRoom -> [Path South SouthRoom, Path West Corridor]
	otherwise -> []

locationShortDesc' :: Room -> String
locationShortDesc' room = case room of
	SouthRoom -> "You are standing in the middle room at the wooden table."
	NorthRoom -> "This is big light room."
	otherwise -> "Invalid room."
	
locationLongDesc' :: Room -> String
locationLongDesc' room = case room of
	SouthRoom -> "Room looks nice: small, clean, beauty. There is phone and papers on the big wooden table.  It is rainy and dark behind the window. A lightnings beat to the lighthouse on a mountain."
	NorthRoom -> "SouthRoom is the big nice place with many lamps on the walls."
	otherwise -> "Invalid room."
	
locationObjects' :: Room -> Objects
locationObjects' room = case room of
	SouthRoom -> [object homeDrawer, object homePhone, object homeUmbrella1, object homeTable]
	NorthRoom -> [object homeUmbrella2]
	otherwise -> []

----------------------
	
	
location :: Room -> Location
location room = Location {
							locRoom = room,
							locPaths = locationPaths' room,
							locShortDesc = locationShortDesc' room,
							locLongDesc = locationLongDesc' room,
							locObjects = locationObjects' room
							}

initWorld :: GameState
initWorld = GameState {
	gsLocations = [location SouthRoom, location NorthRoom],
	gsCurrentRoom = SouthRoom,
	gsRoomLongDescribed = [SouthRoom],
	gsInventory = []
}

lookAround :: Room -> Objects -> String
lookAround room objects = (locLongDesc . location $ room) ++ (describeObjects objects)

isRoomLongDescribed :: Rooms -> Room -> Bool
isRoomLongDescribed = flip elem

describeLocation :: Bool -> Room -> Objects -> String
describeLocation False room objects = (locLongDesc . location $ room) ++ describeObjects objects
describeLocation True  room objects = (locShortDesc . location $ room) ++ describeObjects objects

locationWithoutObject loc obj = Location {
		locRoom = locRoom loc,
		locPaths = locPaths loc,
		locShortDesc = locShortDesc loc,
		locLongDesc = locLongDesc loc,
		locObjects = filter (/=obj) (locObjects loc)}

locationsWithoutObject :: Locations -> Room -> Object -> Locations
locationsWithoutObject [] _ _ = []
locationsWithoutObject locs room obj = [cl : unfls | cl <- changedLocation locs, unfls <- filter (\z -> locRoom z /= room) locs]
	where
		filteredLocations = filter (\x -> locRoom x == room)
		changedLocation ls = case null . filteredLocations ls of
			True -> []
			False -> locationWithoutObject (head filteredLocations) obj

--locationsWithoutObject (l:locs) room obj = if locRoom l == room then locationWithoutObject l obj : locs else l : locationsWithoutObject locs room obj