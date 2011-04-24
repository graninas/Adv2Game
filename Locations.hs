module Locations where

import Types


locationDescription :: Room -> Location
locationDescription loc = case loc of
	SouthRoom -> Location {
							paths = [Path {dir = North, toLoc = NorthRoom},
									Path {dir = South, toLoc = SouthRoom}],
							shortDesc = "This is small dark room.",
							longDesc = "SouthRoom looks like understair corner."
							}
	NorthRoom -> Location {
							paths = [Path {dir = South, toLoc = SouthRoom},
									Path {dir = West, toLoc = Corridor}],
							shortDesc = "This is big light room.",
							longDesc = "SouthRoom is the big nice place with many lamps on the walls."
							}
	otherwise -> Location [] "Undescribed or unknown location" "Undescribed or unknown location"

getLocationDirections :: Room -> Directions
getLocationDirections loc = []
	
describeLocation :: Room -> String
describeLocation x	| x == SouthRoom = "This is small dark room."
					| x == NorthRoom = "This is big light room."
					| otherwise = "No description for this location."

waltToDirection :: (Room, Direction) -> Room
waltToDirection (oldLoc, x) = NorthRoom

