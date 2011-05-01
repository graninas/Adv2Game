module Directions where

import Types
import Locations
import Tools

directionFromStr :: String -> Direction
directionFromStr x = case upString(x) of
							"NORTH" -> North
							"SOUTH" -> South
							"WEST" -> West
							"EAST" -> East
							_ -> NoDirection					