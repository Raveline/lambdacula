module World
(
    Room(..),
    RoomObject(..),
    getTextForAction,
    findObjectInteraction,
    mapFromRooms,
    getRoomByName,
    Character(..),
    Exit(..),
    Player(..),
    World(..),
    Actionable(..),
    basicMove,
    singleAnswer
)
where

import qualified Data.Map as Map
import Action
import Data.List
import Data.Maybe
import Control.Monad.State
import Control.Applicative

type Conversations = Map.Map String String
type Interactions = Map.Map Action String
type WorldAction = State World [String]

class Actionable f where 
    -- Process an action on this object, return the world 
    -- and a string to tell what happened
    actOn :: f -> Action -> State World [String] 
    -- Check if a string match this object (usually by looking
    -- at aliases).
    match :: String -> f -> Bool

data World = World { player :: Player, currentRoom :: Room, worldRooms :: Map.Map String Room }

data Player = Player { inventory :: [String] }
    deriving (Show)

data Room =    Room { roomName :: String
                 ,description :: String
                 ,objects :: [RoomObject]
                 ,characters :: [Character]
                 ,exits :: [Exit]
                }
    deriving (Show, Eq)

-- Check if a string is one of an alias of one of the object in a room
-- Was refactored to change the return type. 
-- This is way too long and ugly.
-- There MUST be a better way to do this !

data RoomObject = RoomObject {   objectName :: String
                                , objectAliases :: [String], 
                                objectReactions :: Action -> State World [String]} 

findObjectInteraction :: String -> Room -> Maybe (Action -> State World [String])
findObjectInteraction s room = case newWorlds of
                                [x] -> Just x
                                _ -> Nothing
                where 
                    newWorlds = actionedObjects ++ actionedCharacters ++ actionedExits
                    actionedObjects = actOnAll . findAMatch $ objects room
                    actionedCharacters = actOnAll . findAMatch $ characters room
                    actionedExits = actOnAll . findAMatch $ exits room
                    findAMatch :: (Actionable a) => [a] -> [a]
                    findAMatch = filter (match s) 
                    actOnAll :: (Actionable a) => [a] -> [Action -> State World [String]]
                    actOnAll = fmap actOn

instance Show RoomObject where
    show = show . objectName

instance Eq RoomObject where
    (==) r1 r2 = objectName r1 == objectName r2

instance Actionable RoomObject where
    actOn = objectReactions
    match s = (s `elem`) . ((:) <$> objectName <*> objectAliases)

getTextForAction :: RoomObject -> Action -> String 
getTextForAction obj act = "Test" 

data Exit = Exit { exitName :: String
                , exitAliases :: [String] 
                , exitDescription :: String
                , exitActions :: Action -> State World [String]
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

mapFromRooms :: [Room] -> Map.Map String Room
mapFromRooms rs = Map.fromList $ map ((,) <$> roomName <*> id) rs

getRoomByName :: World -> String -> Room
getRoomByName w s =
  fromMaybe (error "Error in the room names") $ Map.lookup s $ worldRooms w


basicMove :: Room -> Action -> State World [String]
basicMove r Move = singleAnswer "HERE PUT A MOVE"
basicMove _ _ = singleAnswer "What on earth are you trying to do ?"

singleAnswer :: String -> State World [String]
singleAnswer = return . (:[])
