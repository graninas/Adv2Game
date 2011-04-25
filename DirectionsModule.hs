module DirectionsModule where

import Types
import Locations
import Tools

directionFromCommand :: String -> Direction
directionFromCommand x = case upString(x) of
							"NORTH" -> North
							"SOUTH" -> South
							"WEST" -> West
							"EAST" -> East
							_ -> NoDirection

directionsToString :: Directions -> String
directionsToString [] = []
directionsToString (dir:dirs) = show dir ++ case null(dirs) of
												True -> []
												False -> ", " ++ directionsToString dirs
							
describeDirections :: Room -> String
describeDirections room = "You can go " ++ directionsToString (getRoomDirections room) ++ "."

getPathDirections :: Paths -> Directions
getPathDirections [] = []
getPathDirections (x:xs) = [dir $ x] ++ getPathDirections xs

getRoomDirections :: Room -> Directions
getRoomDirections room = getPathDirections . getLocationPaths . location $ room
												
walkBy = undefined