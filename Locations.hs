module Locations where

import Types
import Objects
import Paths
import Text.Printf(printf)
import qualified Data.Map as M

--- Data functions ---

locationPaths' :: Room -> Paths
locationPaths' room = case room of
	Home -> [Path North Friend'sYard, Path South Home, Path West TestLoseRoom, Path East Garden]
	Friend'sYard -> [Path South Home, Path West Garden, Path North TestWinRoom]
	Garden -> [Path West Home]
	_ -> []

locationShortDescription' :: Location -> String
locationShortDescription' (Location room _ _) = case room of
	Home -> "You are standing in the middle room at the wooden table."
	Friend'sYard -> "This is big light room."
	Garden -> "You are in the friend's garden. Garden looks well."
	_ -> "Invalid room."

locationLongDescription' :: Location -> String
locationLongDescription' (Location room _ _) = case room of
	Home -> "Room looks nice: small, clean, beauty. There is phone and papers on the big wooden table.  It is rainy and dark behind the window. A lightnings beat to the lighthouse on a mountain."
	Friend'sYard -> "Friend'sYard is the big nice place with many lamps on the walls."
	Garden -> "This is friend's garden. You see very nice trees and flower beds."
	_ -> "Invalid room."

----------- Messages, Errors ------------

successWalkingMsg :: Room -> Direction -> String
successWalkingMsg room dir = printf "You walking %s to %s."  (show dir) (show room)

failureWalkingMsg :: Direction -> String
failureWalkingMsg dir = printf "You can't walk %s." (show dir)

-----------------------------------------

instance Eq Location where
	loc1 == loc2 = locRoom loc1 == locRoom loc2

walk :: Location -> Direction -> Locations -> MaybeLocation
walk (Location _ paths _) dir locs = case pathOnDirection paths dir >>= \x -> getLocation (pathRoom x) locs of
		Just loc -> (Just loc, successWalkingMsg (locRoom loc) dir)
		Nothing -> (Nothing, failureWalkingMsg dir)

locationObjects :: Location -> Objects -> Objects
locationObjects loc os = filter (\x -> locRoom loc == objectRoom x) os

location :: Room -> Location
location room = Location {
							locRoom = room,
							locPaths = locationPaths' room,
							locLongDescribed = False
							}

lookAround :: Location -> Objects -> String
lookAround _ [] = undefined
lookAround loc os = locationLongDescription' loc ++ describeObjects [] (locationObjects loc os)

describeLocation :: Location -> Objects -> (String, Location)
describeLocation loc objects = case locLongDescribed loc of
								False -> (longDescr, loc {locLongDescribed = True})
								True -> (shortDescr, loc)
	where
		longDescr = (locationLongDescription' loc) ++ describeObjects [] (locationObjects loc objects)
		shortDescr = (locationShortDescription' loc) ++ describeObjects [] (locationObjects loc objects)

----------------------------------------------------------------------

getLocation :: Room -> Locations -> Maybe Location
getLocation = M.lookup

updateLocations :: Location -> Locations -> Locations
updateLocations loc = M.update (\x -> if x == loc then Just loc else Nothing) (locRoom loc)