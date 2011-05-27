module Main where

import Types
import Objects
import Locations
import Tools
import GameAction
import Control.Monad.State (get, gets, StateT(..), evalStateT, 
                            put, MonadState(..), liftIO)
import Char(isDigit, isAlpha)
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


parseCommand :: String -> Either String Command
parseCommand [] = Left []
parseCommand str = case parse str of
					Just cmd -> Right cmd
					Nothing -> Left "Can't understand a command."

run' :: InputString -> Maybe InputCommand -> GameState -> GameAction
run' inputStr maybeInputCmd curGS = do
		case maybeInputCmd of
			Nothing -> case parseCommand inputStr of
				Left [] -> ReadUserInput
				Left str -> PrintMessage str
				Right New -> StartNewGame
				Right (Quit str) -> QuitGame str
				Right (Walk dir) -> tryWalk' dir curGS
				Right Inventory -> showInventory' curGS
				Right Look -> look' curGS
				Right (Examine obj) -> tryExamineObject' obj
				Right (Take obj) -> tryTake' obj curGS
				Right Help -> PrintMessage helpMessage
				Right (Weld obj1 obj2) -> tryWeld' obj1 obj2 curGS
				Right (Open obj) -> tryOpen' obj curGS
--			Just (QualifyPickup objects) -> tryTakeS inputStr objects curGS
--			Just (QualifyOpen objects) -> tryOpenS inputStr objects curGS

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