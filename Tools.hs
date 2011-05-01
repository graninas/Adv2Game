module Tools where


import Char (toUpper, toLower)
import System.IO (hFlush, stdout)
import Control.Monad.State ({-get, gets, StateT(..), evalStateT, 
                            put, MonadState(..), -}liftIO, MonadIO(..))
import Types

upString :: String -> String
upString str = map toUpper str

capitalize :: String -> String
capitalize str = unwords (map capitalize' (words str))
	where capitalize' (x:xs) = (toUpper x) : map toLower xs

inputStrCommand = do
				putStr "> "	-- Код взят из Advgame.
				hFlush stdout
				line <- getLine
				return (line)
 
ioOutMsg :: String -> GS ()
ioOutMsg = liftIO . putStrLn