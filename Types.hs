{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where
import Control.Monad (mapM_)
import Control.Monad.State (StateT(..), MonadState(..), MonadIO(..))
							
data Room = SouthRoom | NorthRoom | Corridor | NoRoom
	deriving (Show, Eq)

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
			| Hook
	deriving (Show, Eq, Read)
	
type Item = (ItemName, Integer)

data Object = Object {
	oItem :: Item,
	oName :: String,
	oDescription :: String,
	oPickupFailMsg :: String
} deriving (Eq)

type Objects = [Object]
type Inventory = Objects
type Directions = [Direction]

data Path = Path {
    pathDirection :: Direction,
    pathRoom :: Room
} deriving (Eq, Show)

type Paths = [Path]

data Location = Location {
	locRoom :: Room,
	locPaths :: Paths,
	locShortDescription :: String,
	locLongDescription :: String,
	locObjects :: Objects,
	locLongDescribed :: Bool
} deriving (Eq)

type Locations = [Location]
	
data GameState = GameState {
	gsLocations :: Locations,
	gsCurrentLocation :: Location,
	gsInventory :: Inventory
}

newtype GS a = GS {
	runGameState :: StateT GameState IO a
} deriving (Monad, MonadIO, MonadState GameState)

data Command =
			Walk Direction
			| Look
			| Investigate ItemName
			| Inv ItemName 			-- short version of Investigate
			| Inventory
			| Pickup ItemName		-- pickups if itemName parsed
			| Take String			-- tries pickup object by string
			| Weld ItemName ItemName
			| Quit
			| Help
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