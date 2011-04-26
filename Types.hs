module Types where

data Room = SouthRoom | NorthRoom | Corridor
	deriving (Show, Eq)	-- Show позволяет отобразить имя "Room" или "NorthRoom" встроенными средствами.
						-- Eq позволяет сравнивать эти элементы.
	
data Direction = North | South | West | East | NoDirection
	deriving (Show, Eq)
	
data Action = Walk | Look | NoAction | Quit
	deriving (Show, Eq)
	
data Command = Command {
	commandAction :: Action,
	commandDir :: Direction
} deriving (Eq, Show)

type Directions = [Direction]

data Path = Path {
    pathDir :: Direction,
    pathRoom :: Room
} deriving (Eq, Show)

type Paths = [Path]

data Location = Location {
	paths :: Paths,
	shortDesc :: String,
	longDesc :: String
} deriving (Eq, Show)

data GameSituation = GameSituation {
	gameRoom :: Room,
	gameDescribeShort :: Bool,
	gameDescribeLong :: Bool
} deriving (Eq, Show)

