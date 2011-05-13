module Main where

import Types
import Objects
import Locations
import Tools
import GameState
import Control.Monad.State (get, gets, StateT(..), evalStateT, 
                            put, MonadState(..), liftIO)
import Char(isDigit, digitToInt)

helpMessage :: String
helpMessage = unlines ["Welcome to Adv2Game: Advanced Adventure Game!",
	"Author: Granin A. S.",
	"mailto: graninas@gmail.com",
	"",
	"Game commands:",
	"Walk <Direction>",
	"Look",
	"Investigate <ItemName> or Inv <ItemName>",
	"Inventory or I",
	"Pickup <ItemName> or P <ItemName>",
	"Take <Object>",
	"Quit or Q",
	"Help or H",
	"", "Here <Object> is full name of object and <ItemName> is it's simple name.",
	"For example: 'Broken Phone' - object full name and 'Phone' is it's simple name.",
	"Input is case insensitive."]

parseCommand :: String -> ParseResult
parseCommand [] = (Nothing, [])
parseCommand str = let
					capStrings = capitalize $ str
					capedWords = words capStrings
					wordsAfterCommand = unwords . tail $ capedWords in
						case reads capStrings of
							[(x,"")] -> (Just x, [])
							_ -> case head capedWords of
								"Take" -> (Just (Take wordsAfterCommand), [])
								"Q" -> (Just Quit, "Be seen you...")
								"I" -> (Just Inventory, [])
								"H" -> (Just Help, [])
								"P" -> case wordsAfterCommand of
									[] -> (Nothing, "Pickup what?")
									otherwise -> case reads wordsAfterCommand of
												[(y, "")] -> (Just (Pickup y), [])
												_ -> (Just (Take wordsAfterCommand), [])
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
				(Just (Investigate itmName), _) -> tryInvestigateItem itmName (locationObjects ++ inventory)
				(Just (Pickup itmName), _) -> tryPickup itmName locationObjects curGS
				(Just (Take str), _) ->  tryTake str locationObjects curGS
				(Just (Inv itmName), _) -> tryInvestigateItem itmName (locationObjects ++ inventory)
				(Just Help, _) -> PrintMessage helpMessage
				(Just (Weld itmName1 itmName2), _) -> tryWeld itmName1 itmName2 (locationObjects ++ inventory) curGS
			Just (QualifyPickup objects) -> tryTake inputStr objects curGS

run :: InputString -> Maybe InputCommand -> GS ()
run inputStr oldInputCmd = do
	curGS <- get
	gameAction <- return (run' inputStr oldInputCmd curGS)
	case gameAction of
		QuitGame outMsg -> ioOutMsgGS outMsg >> return ()
		PrintMessage outMsg -> ioOutMsgGS outMsg >> run "" Nothing
		ReadUserInput -> ioInMsgGS >>= \x -> run x Nothing
		ReadMessagedUserInput inOutString newInputCmd -> ioOutMsgGS inOutString >> ioInMsgGS >>= \x -> run x (Just newInputCmd)
		SaveState newState outMsg -> ioOutMsgGS outMsg >> put newState >> run "" Nothing

main :: IO ()
main = do
	_ <- evalStateT (runGameState (run "Look" Nothing)) initGameState
	putStrLn ""