module Locations where

import Types


location :: Location -> (Paths, ShortDescription, LongDescription)
location loc = case loc of
	Room -> (	[Path {dir = North, toLoc = NorthRoom},
				Path {dir = South, toLoc = Room}],
				"This is small dark room.", "Room looks like understair corner.")
	NorthRoom -> (	[Path {dir = South, toLoc = Room},
					Path {dir = West, toLoc = Corridor}],
					"This is big light room.", "Room is the big nice place with many lamps on the walls.")
	otherwise -> ([], "Undescribed or unknown location", "Undescribed or unknown location")


getLocationDirections :: Location -> Directions
getLocationDirections loc = []
	
describeLocation :: Location -> String
describeLocation x	| x == Room = "This is small dark room."
					| x == NorthRoom = "This is big light room."
					| otherwise = "No description for this location."

waltToDirection :: (Location, Direction) -> Location
waltToDirection (oldLoc, x) = NorthRoom

