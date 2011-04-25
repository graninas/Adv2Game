module Tools where


import Char (toUpper, toLower)
import System.IO (hFlush, stdout)

upString :: String -> String
upString str = map toUpper str

inputStrCommand = do
				putStr "> "
				hFlush stdout
				line <- getLine
				return (line)