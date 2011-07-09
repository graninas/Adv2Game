module GameStateEvents where

import Types
import Objects

eventByLocation :: GameStateEventFunc
eventByLocation gs@(GameState _ TestWinRoom _) = WinEvent
eventByLocation gs@(GameState _ TestLoseRoom _) = LoseEvent
eventByLocation _ = NoEvent

winByObject :: GameStateEventFunc
winByObject gs@(GameState _ _ os) | superMagicGreatArtifact `elem` (inventoryObjects os) = WinEvent
winByObject _ = NoEvent

winMessage  = "Congratulations! You win!"
loseMessage = "You lose, sorry."