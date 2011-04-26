module Locations where

import Types


location :: Room -> Location
location room = case room of
	SouthRoom -> Location {
							paths = [Path North NorthRoom,
									Path South SouthRoom],
							shortDesc = "This is small dark room.",
							longDesc = "SouthRoom looks like understair corner."
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


describeLocation :: Room -> String
describeLocation room = shortDesc . location $ room

