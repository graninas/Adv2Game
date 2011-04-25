module Types where

data Room = SouthRoom | NorthRoom | Corridor
	deriving (Show, Eq)	-- Show позволяет отобразить имя "Room" или "NorthRoom" встроенными средствами.
						-- Eq позволяет сравнивать эти элементы.
	
data Direction = North | South | West | East | NoDirection
	deriving (Show, Eq)
	
data Action = Walk | Look
	deriving (Show, Eq)
	
data Command = Command {
	actionCommand :: Action,
	dirCommand :: Direction
} deriving (Eq, Show)

type Directions = [Direction]

data Path = Path {
    dir :: Direction,
    toLoc :: Room
} deriving (Eq, Show)

type Paths = [Path]

data Location = Location {
	paths :: Paths,
	shortDesc :: String,
	longDesc :: String
} deriving (Eq, Show)

