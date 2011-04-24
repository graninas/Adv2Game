module Main where

import Types
import Locations
import System.IO (hFlush, stdout)
import Control.Monad.State (liftIO)

directionsToString :: [Direction] -> String
directionsToString (dir:dirs) = show dir ++ case null(dirs) of
												True -> []
												False -> ", " ++ directionsToString dirs
directionsToString [] = []

describeDirections :: Location -> String
describeDirections loc = "You can go " ++ directionsToString ( getLocationDirections loc)

describeActions :: Location -> String
describeActions _ = "You can do something."

describeGameSituation :: Location -> String
describeGameSituation loc = do
						describeDirections loc
						describeActions loc


getAction = do
				putStr "> "
				hFlush stdout
				line <- getLine
				return (line)

run :: Location -> IO ()
run oldLoc = do
				putStrLn . describeLocation $ oldLoc
				putStrLn . describeDirections $ oldLoc
				putStrLn . describeActions $ Room
				x <- getAction
				case x of
					"Quit" -> return ()
					"q" -> return ()
					otherwise -> run . getLocation $ (oldLoc, x)
			
main :: IO ()
main = run Room