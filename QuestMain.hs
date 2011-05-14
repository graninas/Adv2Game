module Main where

import Types
import Objects
import Locations
import Tools
import GameState
import Control.Monad.State (get, gets, StateT(..), evalStateT, 
                            put, MonadState(..), liftIO)
import Char(isDigit, digitToInt)
import qualified System.IO.Error as SysIOError

saveGame :: GameState -> GS ()
saveGame curGS = liftIO $(writeFile "save.a2g" (show curGS))

helpMessage :: String
helpMessage = unlines ["Welcome to Adv2Game: Advanced Adventure Game!",
	"Author: Granin A. S.",
	"mailto: graninas@gmail.com",
	"",
	"Game commands:",
	"Walk <Direction>",
	"Look",
	"Investigate <ObjectName> or Inv <ObjectName>",
	"Inventory or I",
	"Pickup <ObjectName> or P <ObjectName>",
	"Take <Object>",
	"Quit or Q",
	"Help or H",
	"", "Here <Object> is full name of object and <ObjectName> is it's simple name.",
	"For example: 'Broken Phone' - object full name and 'Phone' is it's simple name.",
	"Input is case insensitive."]


caseCmdTail :: String -> (a -> Command) -> (String -> Command) -> (String, [(a, String)]) -> String -> (Maybe (Command), String)
caseCmdTail doWhatMsg cmdMain cmdAlt cmdTail wordsAfterCommand = case cmdTail of
			([], _) -> (Nothing, doWhatMsg)
			(_, [(y, "")]) -> (Just (cmdMain y), [])
			(_, _) -> (Just (cmdAlt wordsAfterCommand), [])
	
parseCommand :: String -> (Maybe Command, String)
parseCommand [] = (Nothing, [])
parseCommand str =
				let
					capStrings = capitalize $ str
					capedWords = words capStrings
					wordsAfterCommand = unwords . tail $ capedWords
--					cmdTail = (wordsAfterCommand, reads wordsAfterCommand)
				in
					case reads capStrings of
						[(x,[])] -> (Just x, [])		-- 1 вариант, полностью распознанная команда, в остатке нет ничего.
						_ -> case head capedWords of	-- Короткие и строковые команды
							"Q" -> (Just Quit, "Be seen you...")
							"I" -> (Just Inventory, [])
							"H" -> (Just Help, [])
--							"T" -> caseCmdTail "Take what?" (\lCmdMain -> Take lCmdMain) (\lCmdAlt -> Take lCmdAlt) cmdTail wordsAfterCommand-- Изящное решение, как передать конструктор (Pickup, Take) в другую функцию.
							_ -> (Nothing, "Can't understand a command.")

run' :: InputString -> Maybe InputCommand -> GameState -> GameAction
run' inputStr maybeInputCmd curGS = do
		let currentLocation = gsCurrentLocation curGS
		let locationObjects = locObjects currentLocation
		let inventory = gsInventory curGS
		case maybeInputCmd of
			Nothing -> case parseCommand inputStr of
				(Nothing, []) -> ReadUserInput
				(Nothing, str) -> PrintMessage str
				(Just Quit, _) -> QuitGame "Be seen you..."
				(Just (Walk dir), _) -> tryWalk currentLocation dir curGS
				(Just Inventory, _) -> PrintMessage (showInventory inventory)
				(Just Look, _) -> PrintMessage (lookAround currentLocation)
				(Just (Examine itm), _) -> tryExamineItem itm (locationObjects ++ inventory)
				(Just (Take itm), _) -> tryTake itm locationObjects curGS
				(Just Help, _) -> PrintMessage helpMessage
				(Just (Weld itm1 itm2), _) -> tryWeld itm1 itm2 (locationObjects ++ inventory) curGS
			Just (QualifyPickup objects) -> tryTakeS inputStr objects curGS

run :: InputString -> Maybe InputCommand -> GS ()
run inputStr oldInputCmd = do
	curGS <- get
	gameAction <- return (run' inputStr oldInputCmd curGS)
	case gameAction of
		QuitGame outMsg -> ioOutMsgGS outMsg >> return ()
		PrintMessage outMsg -> ioOutMsgGS outMsg >> run "" Nothing
		ReadUserInput -> ioInMsgGS >>= \x -> run x Nothing
		ReadMessagedUserInput inOutString newInputCmd -> ioOutMsgGS inOutString >> ioInMsgGS >>= \x -> run x (Just newInputCmd)
		SaveState newState outMsg -> ioOutMsgGS outMsg >> put newState >> return newState >>= saveGame >> run "" Nothing

loadGame str = case reads str of
				[(x,"")] -> Just x
				_ -> Nothing
		
main :: IO ()
main = do
	str <- catch (readFile "save.a2g") (\_ -> return [])
	(startGameState, msg) <- case str of
			[] -> return (initGameState, "Starting new game...\n")
			_ -> case loadGame str of
				Just gs -> return (gs, "Restoring previous game...\n")
				Nothing -> return (initGameState, "Starting new game...\n")
	putStrLn msg
	_ <- evalStateT (run "Look" Nothing) startGameState
	putStrLn ""