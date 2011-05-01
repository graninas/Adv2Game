module Tools where


import Char (toUpper, toLower)
import System.IO (hFlush, stdout)

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