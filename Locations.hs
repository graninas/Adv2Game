module Locations where

import Types


location :: Room -> Location
location room = case room of
	SouthRoom -> Location {
							paths = [Path North NorthRoom,
									Path South SouthRoom],
							shortDesc = "You are standing in the middle room with wooden table.",
							longDesc = "Room looks nice: small, clean, beauty. There is phone and papers on the big wooden table.  It is rainy and dark behind the window. Lightings beats to the lighthouse on a mountain."
							}
	NorthRoom -> Location {
							paths = [Path South SouthRoom,
									Path West Corridor],
							shortDesc = "This is big light room.",
							longDesc = "SouthRoom is the big nice place with many lamps on the walls."
							}
	otherwise -> Location [] "Undescribed or unknown location" "Undescribed or unknown location"


	
getLocationPaths :: Location -> Paths
getLocationPaths loc = paths $ loc

describeLocation :: Room -> Bool -> Bool -> Maybe String
describeLocation room isShort isLong =	if isShort then Just . shortDesc . location $ room
										else if isLong then Just . longDesc . location $ room
										else Nothing

