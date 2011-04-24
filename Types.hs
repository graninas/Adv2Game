module Types where

data Location = Room | NorthRoom
	deriving (Show, Eq)
	
data Direction = North | South | West | East
	deriving (Show)

data Action = Go | LookMore