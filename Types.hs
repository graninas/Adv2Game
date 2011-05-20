module Types where
import Control.Monad (mapM_)
import Control.Monad.State (StateT(..), MonadState(..), MonadIO(..), evalStateT)
							
data Room = SouthRoom | NorthRoom | Corridor | NoRoom
	deriving (Eq, Show, Read)

type Rooms = [Room]
type LongDescribedRooms = Rooms

data Direction = North | South | West | East | NoDirection
	deriving (Show, Eq, Read)

data Item =
		Combined Item Item
		| Phone
		| Drawer
		| Umbrella
		| Rope
		| Hook
		| Table
		| Lighter
			
	deriving (Eq, Show, Read)
	
data ContainerState =
			NotContainer
			| Opened
			| Closed
	deriving (Eq, Show, Read)

type ObjectName = String

data Object = Object {
	objectItem :: Item,
	objectName :: ObjectName,
	objectContainerState :: ContainerState,
	objectContains :: Objects
} deriving (Eq, Show, Read)

type ObjectIdentifier = (ObjectName, Item)

type Objects = [Object]
type Inventory = Objects
type Directions = [Direction]

data Path = Path {
    pathDirection :: Direction,
    pathRoom :: Room
} deriving (Eq, Show, Read)

type Paths = [Path]

data Location = Location {
	locRoom :: Room,
	locPaths :: Paths,
	locShortDescription :: String,
	locLongDescription :: String,
	locObjects :: Objects,
	locLongDescribed :: Bool
} deriving (Eq, Show, Read)

type Locations = [Location]
	
data GameState = GameState {
	gsLocations :: Locations,
	gsCurrentLocation :: Location,
	gsInventory :: Inventory
} deriving (Show, Read)

type GS a = (StateT GameState IO a)

data Command =
			Walk Direction
			| Look
			| Examine Item
			| ExamineS String
			| Inventory
			| Take Item
			| TakeS String
			| Weld Item Item
			| Open Item
			| Quit
			| Help
	deriving (Eq, Show, Read)
	
data InputCommand = QualifyPickup Objects

type InputString = String
type OutputMessage = String
type InputOutputString = String

data GameAction =
				PrintMessage OutputMessage
				| QuitGame OutputMessage
				| ReadUserInput
				| ReadMessagedUserInput InputOutputString InputCommand
				| SaveState GameState OutputMessage