module Lambdacula.World
(
    Room(..),
    RoomObject(..),
    findObjectInteraction,
    Character(..),
    Exit(..),
    Player(..),
    World(..),
    Actionable(..),
    singleAnswer,
    WorldAction,
    worldRooms,
    currentRoom
)
where

import qualified Data.Map as Map
import Lambdacula.Action as Act
import Data.Char
import Data.List
import Data.Maybe
import Control.Monad.State
import Control.Applicative
import Control.Lens


type Conversations = Map.Map String String
type Interactions = Map.Map Act.Action String
type WorldAction = State World [String]

class Actionable f where 
    -- Process an action on this object, return the world 
    -- and a string to tell what happened
    actOn :: f -> Act.Action -> WorldAction 
    -- Check if a string match this object (usually by looking
    -- at aliases).
    match :: String -> f -> Bool

data Room =    Room { _roomName :: String
                 ,_description :: String
                 ,_objects :: [RoomObject]
                 ,_characters :: [Character]
                 ,_exits :: [Exit]
                }
  deriving (Show, Eq)

data World = World { _player :: Player, 
                    _currentRoom :: Room, 
                    _worldRooms :: [Room] }

worldRooms :: Simple Lens World [Room]
worldRooms = lens (\w -> _worldRooms w) (\w rs -> World (_player w) (_currentRoom w) rs)
currentRoom :: Simple Lens World Room
currentRoom = lens (\w -> _currentRoom w) (\w cr -> World (_player w) cr (_worldRooms w))


data Player = Player { inventory :: [String] }
    deriving (Show)

data RoomObject = RoomObject { 
                     objectName :: String
                    ,objectAliases :: [String]
                    ,objectReactions :: Act.Action -> WorldAction }

instance Show RoomObject where
    show = show . objectName

instance Eq RoomObject where
    (==) r1 r2 = objectName r1 == objectName r2

instance Actionable RoomObject where
    actOn = objectReactions
    match s = (s `elem`) . ((:) <$> objectName <*> objectAliases)

data Exit = Exit { exitName :: String
                , exitAliases :: [String] 
                , exitDescription :: String
                , exitActions :: Act.Action -> WorldAction 
                , exitOpened :: Bool } 

instance Show Exit where
    show e = exitName e ++ " : " ++ exitDescription e

instance Eq Exit where
    (==) e1 e2 = exitName e1 == exitName e2

instance Actionable Exit where
    actOn = exitActions
    match s = (s `elem`) . ((:) <$> exitName <*> exitAliases)

data Character = Character {    name :: String, 
                                aliases :: [String], 
                                topics :: Conversations, 
                                npcReactions :: Interactions } 
                deriving (Show, Eq)

instance Actionable Character where
    actOn char action = error "Not implemented"
    match s = (s `elem`) . ((:) <$> name <*> aliases)

data ObjectStatus = Open | Closed | Nada



-- Given a string and a room, try to find a RoomObject,
-- an Exit or a Character matching the string. Send back
-- a potential reaction function.
findObjectInteraction :: String -> Room -> Maybe (Act.Action -> WorldAction) 
findObjectInteraction s room = case newWorlds of
                                [x] -> Just x
                                _ -> Nothing
            where 
                newWorlds = actionedObjects ++ actionedCharacters ++ actionedExits
                actionedObjects = actOnAll . findAMatch $ _objects room
                actionedCharacters = actOnAll . findAMatch $ _characters room
                actionedExits = actOnAll . findAMatch $ _exits room
                findAMatch :: (Actionable a) => [a] -> [a]
                findAMatch = filter (match s) 
                actOnAll :: (Actionable a) => [a] -> [Act.Action -> WorldAction] 
                actOnAll = fmap actOn

-- Given a string, will return the World "as it is" and the string.
singleAnswer :: String -> WorldAction 
singleAnswer = return . (:[])
