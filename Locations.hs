module Locations where


import Types

describeLocation :: Location -> String
describeLocation x	| x == Room = "This is small dark room."
					| x == NorthRoom = "This is big light room."
					| otherwise = "No description for this location."

					
getLocationDirections :: Location -> [Direction]
getLocationDirections loc = case loc of
		Room -> [North, South]
		NorthRoom -> [South, West]
					
getLocation :: (Location, String) -> Location
getLocation (oldLoc, x) = case x of
							"North" -> NorthRoom
							_ -> oldLoc