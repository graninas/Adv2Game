module Types where
import Control.Monad (mapM_)
import Control.Monad.State (StateT(..), MonadState(..), MonadIO(..), evalStateT)
import qualified Data.Map as M

type MaybeSomething a = (Maybe a, String)


data Room = InventoryRoom | SouthRoom | NorthRoom | Corridor | NoRoom
	deriving (Eq, Show, Read, Ord)

type Rooms = [Room]
type LongDescribedRooms = Rooms

data Direction = North | South | West | East | NoDirection
	deriving (Eq, Show, Read)

type Directions = [Direction]

data OpenCloseState = Opened | Closed
	deriving (Eq, Show, Read)

type ObjectName = String

data Object =
		Object {
			objectName :: ObjectName,
			objectRoom :: Room
		}
		| Container {
			objectName :: ObjectName,
			objectContainerState :: OpenCloseState,
			objectContents :: Objects,
			objectRoom :: Room
		}
		| Complex {
			objectName :: ObjectName,
			objectComponent1 :: Object,
			objectComponent2 :: Object,
			objectRoom :: Room
		}
		deriving (Show, Read, Eq)
	
type Objects = [Object]
type Components = Objects
type MaybeWeldedObject = Maybe (Object, String)
type Welder = Components -> MaybeWeldedObject

data Path = Path {
    pathDirection :: Direction,
    pathRoom :: Room
} deriving (Eq, Show, Read)

type Paths = [Path]

data Location = Location {
	locRoom :: Room,
	locPaths :: Paths,
	locLongDescribed :: Bool
} deriving (Show, Read)

type Locations = M.Map Room Location
type MaybeLocation = MaybeSomething Location
	
data GameState = GameState {
	gsLocations :: Locations,
	gsCurrentRoom :: Room,
	gsObjects :: Objects
} deriving (Show, Read)

type GS a = (StateT GameState IO a)

data Command =
			Walk Direction
			| Look
			| Examine Object
			| Inventory
			| Take Object
			| Weld Object Object
			| Open Object
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
	open :: a -> MaybeSomething a
	close :: a -> MaybeSomething a
	showStated :: a -> String
	showContents :: a -> String
