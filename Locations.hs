module Locations where

import Types


location :: Room -> Location
location room = case room of
	SouthRoom -> Location {
							locRoom = SouthRoom,
							locPaths = [Path North NorthRoom, Path South SouthRoom],
							locShortDesc = "You are standing in the middle room with wooden table.",
							locLongDesc = "Room looks nice: small, clean, beauty. There is phone and papers on the big wooden table.  It is rainy and dark behind the window. Lightings beats to the lighthouse on a mountain.",
							locObjects = [Drawer, Phone, Table]
							}
	NorthRoom -> Location {
							locRoom = NorthRoom,
							locPaths = [Path South SouthRoom, Path West Corridor],
							locShortDesc = "This is big light room.",
							locLongDesc = "SouthRoom is the big nice place with many lamps on the walls.",
							locObjects = []
							}
	otherwise -> Location { locRoom = NoRoom, locPaths = [], locShortDesc = "Invalid room.", locLongDesc = "Invalid room", locObjects = [] }

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
isRoomLongDescribed rooms room = room `elem` rooms

describeObjects :: Objects -> String
describeObjects [] = []
describeObjects objects = "\nThere are some objects here: " ++ show objects

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
locationsWithoutObject (l:locs) room obj = if locRoom l == room then locationWithoutObject l obj : locs else l : locationsWithoutObject locs room obj