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
		| Diary
			
	deriving (Show, Read, Ord)
	
instance Eq Item where
	(Combined i1 i2) == (Combined i3 i4) = i1 == i3 && i2 == i4
	x1 == x2 = x1 `compare` x2 == EQ

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
	objectContents :: Objects
} deriving (Show, Read)

instance Eq Object where
	(Object item1 name1 _ _)  == (Object item2 name2 _ _) = item1 == item2 && name1 == name2

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
			| New
			| Quit String
			| Help
	deriving (Eq, Show, Read)
	
data InputCommand =
		QualifyPickup Objects
		| QualifyOpen Objects

type InputString = String
type OutputMessage = String
type InputOutputString = String

data GameAction =
				PrintMessage OutputMessage
				| QuitGame OutputMessage
				| ReadUserInput
				| ReadMessagedUserInput InputOutputString InputCommand
				| SaveState GameState OutputMessage
				| StartNewGame
				
class Openable a where
	open :: a -> Either String a
	close :: a -> Either String a
	isOpened :: a -> Bool
	isClosed :: a -> Bool
	showStated :: a -> String
	showContents :: a -> String
