{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where
import Control.Monad (mapM_)
import Control.Monad.State (StateT(..), MonadState(..), MonadIO(..))
							
data Room = SouthRoom | NorthRoom | Corridor | NoRoom
	deriving (Show, Eq)	-- Show позволяет отобразить имя "SouthRoom" или "NorthRoom" встроенными средствами.
						-- Eq позволяет сравнивать эти элементы.
type Rooms = [Room]
type LongDescribedRooms = Rooms

data Direction = North | South | West | East | NoDirection
	deriving (Show, Eq, Read)
	
data ItemName =
			Phone
			| Table
			| Drawer
			| Umbrella
	deriving (Show, Eq, Read)
	
type Item = (ItemName, Integer)

data Object = Object {
	oItem :: Item,
	oName :: String,
	oDescription :: String,
	oPickupFailMsg :: String
} deriving (Eq)

type Objects = [Object]
type InventoryObjects = Objects
type Directions = [Direction]

data Path = Path {
    pathDir :: Direction,
    pathRoom :: Room
} deriving (Eq, Show)

type Paths = [Path]

data Location = Location {
	locRoom :: Room,
	locPaths :: Paths,
	locShortDesc :: String,
	locLongDesc :: String,
	locObjects :: Objects
} deriving (Eq)

type Locations = [Location]

data Command =
			Walk Direction
			| Look
			| Investigate ItemName
			| Inventory
			| Go Direction
			| Pickup ItemName
			| Quit
			| Help
	deriving (Eq, Show, Read)

data GameAction = PrintMessage | QuitGame | ReadUserInput

type GameActionResult = (GameAction, String)

data GameState = GameState {
	gsLocations :: Locations,
	gsCurrentRoom :: Room,
	gsRoomLongDescribed :: LongDescribedRooms, -- Если длинное описание уже выводилось, то второй раз оно не будет выводиться. Только по команде Look. В списке gsRoomLongDescribed содержатся уже описанные комнаты.
	gsInvObjects :: InventoryObjects
}

newtype GS a = GS {
	runGameState :: StateT GameState IO a
} deriving (Monad, MonadIO, MonadState GameState)