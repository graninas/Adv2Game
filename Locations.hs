module Locations where

import Types


location :: Room -> Location
location room = case room of
	SouthRoom -> Location {
							locPaths = [Path North NorthRoom,
									Path South SouthRoom],
							locShortDesc = "You are standing in the middle room with wooden table.",
							locLongDesc = "Room looks nice: small, clean, beauty. There is phone and papers on the big wooden table.  It is rainy and dark behind the window. Lightings beats to the lighthouse on a mountain.",
							locLongDescribed = False
							}
	NorthRoom -> Location {
							locPaths = [Path South SouthRoom,
									Path West Corridor],
							locShortDesc = "This is big light room.",
							locLongDesc = "SouthRoom is the big nice place with many lamps on the walls.",
							locLongDescribed = False
							}
	otherwise -> Location [] "Undescribed or unknown location" "Undescribed or unknown location" False

initWorld :: GameState
initWorld = GameState {
	gsWorldMap = [location SouthRoom, location NorthRoom],
	gsCurrentRoom = SouthRoom
}

