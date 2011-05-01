{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where
import Control.Monad (mapM_)
import Control.Monad.State (StateT(..), MonadState(..), MonadIO(..))
							
data Room = SouthRoom | NorthRoom | Corridor
	deriving (Show, Eq)	-- Show позволяет отобразить имя "SouthRoom" или "NorthRoom" встроенными средствами.
						-- Eq позволяет сравнивать эти элементы.
	
data Direction = North | South | West | East | NoDirection
	deriving (Show, Eq, Read)
	
data Object =	Phone |
				Table |
				Drawer	-- Выдвижной ящик стола
	deriving (Show, Eq, Read)

data Command =
			Walk Direction
			| Look
			| Investigate Object
			| Go Direction
			| Quit
			| ErrorCommand
	deriving (Eq, Show, Read)

type Directions = [Direction]

data Path = Path {
    pathDir :: Direction,
    pathRoom :: Room
} deriving (Eq, Show)

type Paths = [Path]

data Location = Location {
	locPaths :: Paths,
	locShortDesc :: String,
	locLongDesc :: String,
	locLongDescribed :: Bool	-- Если длинное описание уже выводилось, то второй раз оно не будет выводиться. Только по команде Look.
} deriving (Eq, Show)


-- Новый вариант состояния игры. Создан по примеру Advgame.
data Result = Won | Lost | ContinueGame | QuitGame
    deriving (Eq)

data GameState = GameState {
	gsWorldMap :: [Location],
	gsCurrentRoom :: Room} 
	deriving (Show)

newtype GS a = GS { runGameState :: StateT GameState IO a }
    deriving (Monad, MonadIO, MonadState GameState)