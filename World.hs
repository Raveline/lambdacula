module World
(
    Room(..),
    RoomObject(..),
    getTextForAction,
    findObject,
    Character(..),
    Player(..),
    World(..)
)
where

import qualified Data.Map as Map
import Action
import Data.List

type Conversations = Map.Map String String
type Interactions = Map.Map Action String

data World = World { player :: Player, currentRoom :: Room }

data Player = Player { inventory :: [String] }
    deriving (Show)

data Room =    Room { roomName :: String
                 ,description :: String
                 ,objects :: [RoomObject]
                 ,characters :: [Character]
                 ,exit :: [Exit]
                }
    deriving (Show)

-- Check if a string is one of an alias of one of the object in a room
findObject :: String -> Room -> Maybe RoomObject
findObject s room = find (isObject s) (objects room)
    where   isObject :: String -> RoomObject -> Bool
            isObject s o = s `elem` (objectAliases o)

-- Object don't have a single name, because they could be many, many things.
data RoomObject = RoomObject { objectName :: String, objectAliases :: [String], objectReactions :: Interactions } deriving (Show, Eq)

getTextForAction :: RoomObject -> Action -> String
getTextForAction obj act = Map.findWithDefault (defaut obj) act (objectReactions obj)
        where defaut o = "You can't do that to " ++ objectName o ++ " !"

data Exit = Exit { exitDescription :: String, destination :: String } deriving (Show)
data Character = Character {name :: String, aliases :: [String], topics :: Conversations, npcReactions :: Interactions } deriving (Show)


