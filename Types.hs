module Types where
import Control.Monad (mapM_)
import Control.Monad.State (StateT(..), MonadState(..), MonadIO(..), evalStateT)
							
data Room = SouthRoom | NorthRoom | Corridor | NoRoom
	deriving (Eq, Show, Read)

type Rooms = [Room]
type LongDescribedRooms = Rooms

data Direction = North | South | West | East | NoDirection
	deriving (Show, Eq, Read)

data ItemName =
			Phone
			| Table
			| Drawer
			| Umbrella
			| Rope
			| TiedRope
			| Hook
	deriving (Show, Eq, Read)

type ObjectID = (String, ItemName)

data Object = Object {
	objectID :: ObjectID
} deriving (Eq, Show, Read)

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
			| Go Direction
			| Look
			| Investigate ItemName
			| Inv ItemName 			-- short version of Investigate
			| Inventory
			| Pickup ItemName		-- pickups if itemName parsed
			| Take String			-- tries pickup object by string
			| Weld ItemName ItemName
			| Open ItemName
			| OpenO String
			| Quit
			| Help
			| NoCommand
	deriving (Eq, Show, Read)

data InputCommand = QualifyPickup Objects
	
type ParseResult = (Maybe Command, String)

type InputString = String
type OutputMessage = String
type InputOutputString = String

data GameAction =
				PrintMessage OutputMessage
				| QuitGame OutputMessage
				| ReadUserInput
				| ReadMessagedUserInput InputOutputString InputCommand
				| SaveState GameState OutputMessage