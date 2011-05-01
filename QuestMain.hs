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

seeAround = locLongDesc . location

run :: GS Result
run = do
	curGS <- get
	strCmd <- liftIO inputStrCommand
	let parsedCmd = parseCommand strCmd
	case parsedCmd of
		Just Quit -> return QuitGame
		Just Look -> (liftIO . putStrLn . seeAround . gsCurrentRoom $ curGS) >> run
		Nothing -> (liftIO $ putStrLn $ show $ parsedCmd) >> run

main :: IO ()
main = do
	x <- evalStateT (runGameState run) initWorld
	putStrLn "End."