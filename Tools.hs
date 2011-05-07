module Tools where


import Char (toUpper, toLower)
import System.IO (hFlush, stdout, IO(..))
import Control.Monad.State ({-get, gets, StateT(..), evalStateT, 
                            put, MonadState(..), -}liftIO, MonadIO(..))
import Types

upString :: String -> String
upString str = map toUpper str

capitalize :: String -> String
capitalize = unwords . map capitalize' . words
	where capitalize' (x:xs) = (toUpper x) : map toLower xs

inputStrCommand :: IO String
inputStrCommand = do
				putStr "> "	-- Код взят из Advgame.
				hFlush stdout
				line <- getLine
				return (line)
 
ioOutMsgGS :: String -> GS ()
ioOutMsgGS = liftIO . putStrLn

ioInMsgGS :: GS String
ioInMsgGS = liftIO inputStrCommand

ioOutMsgIO :: String -> IO ()
ioOutMsgIO = putStrLn