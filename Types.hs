module Types where

data Location = Room | NorthRoom | Corridor
	deriving (Show, Eq)	-- Show позволяет отобразить имя "Room" или "NorthRoom" встроенными средствами.
						-- Eq позволяет сравнивать эти элементы.
	
data Direction = North | South | West | East | NoDirection
	deriving (Show, Eq)


type ShortDescription = String	-- Задание синонима для строки.
type LongDescription = String

data Path = Path {
    dir :: Direction,
    toLoc :: Location
} deriving (Eq, Show)

type Paths = [Path]
type Directions = [Direction]