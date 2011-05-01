module Locations where

import Types


location :: Room -> Location
location room = case room of
	SouthRoom -> Location {
							locPaths = [Path North NorthRoom, Path South SouthRoom],
							locShortDesc = "You are standing in the middle room with wooden table.",
							locLongDesc = "Room looks nice: small, clean, beauty. There is phone and papers on the big wooden table.  It is rainy and dark behind the window. Lightings beats to the lighthouse on a mountain."
							}
	NorthRoom -> Location {
							locPaths = [Path South SouthRoom, Path West Corridor],
							locShortDesc = "This is big light room.",
							locLongDesc = "SouthRoom is the big nice place with many lamps on the walls."
							}
	otherwise -> Location [] "Undescribed or unknown location" "Undescribed or unknown location"

initWorld :: GameState
initWorld = GameState {
	gsWorldMap = [location SouthRoom, location NorthRoom],
	gsCurrentRoom = SouthRoom,
	gsRoomLongDescribed = [SouthRoom]
}

lookAround :: Room -> String
lookAround = locLongDesc . location

isRoomLongDescribed :: Rooms -> Room -> Bool
isRoomLongDescribed rooms room = room `elem` rooms

describeLocation :: Bool -> Room -> String
describeLocation False = locLongDesc . location
describeLocation True = locShortDesc . location