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
	"Look or L",
	"Examine <Item> or E <Item>",
	"Inventory or I",
	"Take <Item> or T <Item>",
	"Weld <Item> <Item>",
	"Quit or Q",
	"Help or H",
	"", "Here <Item> is object's simple name.",
	"For example: 'Phone' is object's simple name.",
	"Input is case insensitive."]

-- Изящное решение, как передать конструктор (Pickup, Take) в другую функцию.
-- Функция принимает короткую команду и пытается найти после этой команды объекты.
-- Допускает два конструктора над короткой командой: главный (cmdMain) и альтернативный (cmdAlt).
-- Альтернативные конструкторы команд (оканчиваются на S) указывают, что объекты будут распознаны в строке где-то в другом месте.
-- Если передана только короткая команда, выдает сообщение (doWhatMsg).
caseCmdTail :: String -> (a -> Command) -> (String -> Command) -> (String, [(a, String)]) -> Either String Command
caseCmdTail doWhatMsg cmdMain cmdAlt cmdTail = case cmdTail of
			(_, [(y, "")]) -> Right (cmdMain y)
			([], _) -> Left doWhatMsg
			(wordsAfterCommand, []) -> Right (cmdAlt wordsAfterCommand)
	
parseCommand :: String -> Either String Command
parseCommand [] = Left []
parseCommand str =
				let
					capStrings = capitalize $ str
					capedWords = words capStrings
					wordsAfterCommand = unwords . tail $ capedWords
					cmdTail = (wordsAfterCommand, reads wordsAfterCommand)
				in
					case reads capStrings of
						[(x,[])] -> Right x				-- 1 вариант, полностью распознанная команда, в остатке нет ничего.
						_ -> case head capedWords of	-- Несколько вариантов, или есть остаток. Распознаются короткие и строковые команды
							"Q" -> Right (Quit "Be seen you...")
							"I" -> Right Inventory
							"H" -> Right Help
							"E" -> caseCmdTail "Examine what?" Examine ExamineS cmdTail
							"T" -> caseCmdTail "Take what?" Take TakeS cmdTail
							"L" -> Right Look
							_ -> Left "Can't understand a command."

run' :: InputString -> Maybe InputCommand -> GameState -> GameAction
run' inputStr maybeInputCmd curGS = do
		let currentLocation = gsCurrentLocation curGS
		let locationObjects = locObjects currentLocation
		let inventory = gsInventory curGS
		case maybeInputCmd of
			Nothing -> case parseCommand inputStr of
				Left [] -> ReadUserInput
				Left str -> PrintMessage str
				Right New -> StartNewGame
				Right (Quit str) -> QuitGame str
				Right (Walk dir) -> tryWalk currentLocation dir curGS
				Right Inventory -> PrintMessage (showInventory inventory)
				Right Look -> PrintMessage (lookAround currentLocation)
				Right (Examine itm) -> tryExamineItem itm (locationObjects ++ inventory)
				Right (Take itm) -> tryTake itm locationObjects curGS
				Right (TakeS str) -> tryTakeS str locationObjects curGS
				Right Help -> PrintMessage helpMessage
				Right (Weld itm1 itm2) -> tryWeld itm1 itm2 (locationObjects ++ inventory) curGS
				Right (Open itm) -> tryOpen itm locationObjects inventory curGS
			Just (QualifyPickup objects) -> tryTakeS inputStr objects curGS
			Just (QualifyOpen objects) -> tryOpenS inputStr objects curGS

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
		StartNewGame -> put initGameState >> ioOutMsgGS "Starting new game...\n" >> run "Look" Nothing

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