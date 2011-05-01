module Main where

import Types
import Locations
import Directions
import Tools
import Control.Monad.State (get, gets, StateT(..), evalStateT, 
                            liftIO, put, MonadState(..), MonadIO(..))
			
parseCommand :: String -> Maybe Command
parseCommand str = case reads capStrings of
					[(x,"")] -> Just x
					_ -> case capStrings of
						"Q" -> Just Quit
						_ -> Nothing
	where capStrings = capitalize $ str

canWalk :: GameState -> Direction -> Maybe Room
canWalk curGS = roomOnDirection (locPaths . location . gsCurrentRoom $ curGS)

tryWalk dir = do
	curGS <- get
	case canWalk curGS dir of
		Just room -> put (GameState {gsWorldMap = (gsWorldMap curGS), gsCurrentRoom = room}) >> return ContinueGame
		Nothing -> return ContinueGame

run :: GS Result
run = do
	curGS <- get
	strCmd <- liftIO inputStrCommand
	let parsedCmd = parseCommand strCmd
	case parsedCmd of
		Just Quit -> return QuitGame
		Just Look -> (liftIO . putStrLn . lookAround . gsCurrentRoom $ curGS) >> run
		Just (Go dir) -> (tryWalk dir) >> run
		Just (Walk dir) -> (tryWalk dir) >> run
		Nothing -> (liftIO . putStrLn . show $ parsedCmd) >> run

main :: IO ()
main = do
	putStrLn . lookAround . gsCurrentRoom $ initWorld
	x <- evalStateT (runGameState run) initWorld
	putStrLn "End."