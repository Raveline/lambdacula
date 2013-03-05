module World
(
    Room(..),
    RoomObject(..),
    Character(..)
)
where

import qualified Data.Map as Map
import Action

type Conversations = Map.Map String String
type Interactions = Map.Map Action String

data Room =    Room { roomName :: String
                 ,description :: String
                 ,objects :: [RoomObject]
                 ,characters :: [Character]
                 ,exit :: [Exit]
                }
    deriving (Show)

data RoomObject = RoomObject { objectName :: String, objectReactions :: Interactions } deriving (Show)
data Exit = Exit { exitDescription :: String, destination :: String } deriving (Show)
data Character = Character {name :: String, aliases :: [String], topics :: Conversations, npcReactions :: Interactions } deriving (Show)
