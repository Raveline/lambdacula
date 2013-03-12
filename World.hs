module World
(
    Room(..),
    RoomObject(..),
    getTextForAction,
    findObject,
    mapFromRooms,
    getRoomByName,
    Character(..),
    Exit(..),
    Player(..),
    World(..),
    Actionable(..),
    singleAnswer
)
where

import qualified Data.Map as Map
import Action
import Data.List
import Control.Monad.State

type Conversations = Map.Map String String
type Interactions = Map.Map Action String

data World = World { player :: Player, currentRoom :: Room, worldRooms :: Map.Map String Room }

data Player = Player { inventory :: [String] }
    deriving (Show)

data Room =    Room { roomName :: String
                 ,description :: String
                 ,objects :: [RoomObject]
                 ,characters :: [Character]
                 ,exit :: [Exit]
                }
    deriving (Show, Eq)

-- Check if a string is one of an alias of one of the object in a room
findObject :: String -> Room -> Maybe RoomObject
findObject s room = find (isObject s) (objects room)
    where   isObject :: String -> RoomObject -> Bool
            isObject s o = s `elem` (objectAliases o)

data RoomObject = RoomObject {   objectName :: String
                                , objectAliases :: [String], 
                                objectReactions :: (Action -> State World [String])} 

instance Show RoomObject where
    show = show . objectName

instance Eq RoomObject where
    (==) r1 r2 = (objectName r1) == (objectName r2)

getTextForAction :: RoomObject -> Action -> String
getTextForAction obj act = "Test" 

data Exit = Exit { exitName :: String
                , exitAliases :: [String] 
                , exitDescription :: String
                , destination :: String
                , exitOpened :: Bool } deriving (Show, Eq)

data Character = Character {    name :: String, 
                                aliases :: [String], 
                                topics :: Conversations, 
                                npcReactions :: Interactions } 
                deriving (Show, Eq)

mapFromRooms :: [Room] -> Map.Map String Room
mapFromRooms rs = Map.fromList $ map (\r -> (roomName r, r)) rs

getRoomByName :: World -> String -> Room
getRoomByName w s = case Map.lookup s $ worldRooms w of
                        Nothing -> error "Error in the room names"
                        Just r -> r

class Actionable f where 
    actOn :: f -> Action -> State World [String] 

instance Actionable RoomObject where
    actOn obj action = (reaction action)
        where reaction = objectReactions obj

singleAnswer :: String -> State World [String]
singleAnswer s = state $ (,) [s]
