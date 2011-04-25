module Locations where

import Types


location :: Room -> Location
location loc = case loc of
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

getPathDirections :: Paths -> Directions
getPathDirections [] = []
getPathDirections (x:xs) = [dir $ x] ++ getPathDirections xs
	
getLocationPaths :: Location -> Paths
getLocationPaths loc = paths $ loc

getRoomDirections :: Room -> Directions
getRoomDirections room = getPathDirections . getLocationPaths . location $ room
	
describeLocation :: Room -> String
describeLocation x = shortDesc $ (location (x))

walkToDirection :: (Room, Direction) -> Room
walkToDirection (oldRoom, x) = NorthRoom

