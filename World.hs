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
import Control.Monad.State

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
findObjectInteraction :: String -> Room -> Maybe (Action -> State World [String])
findObjectInteraction s room
    | isNotOnlyMatch matchObjects matchCharacters matchExits = Nothing -- To replace by an "AMBIGUOUS MESSAGE"
    | matchObjects /= [] = returnMatch matchObjects
    | matchCharacters /= [] = returnMatch matchCharacters
    | matchExits /= [] = returnMatch matchExits
    | otherwise = Nothing
    where
        matchObjects = findAMatch s (objects room)
        matchCharacters = findAMatch s (characters room)
        matchExits = findAMatch s (exits room) 
        findAMatch s = filter (match s)
        isNotOnlyMatch :: [a] -> [b] -> [c] -> Bool
        isNotOnlyMatch a b c = length a + length b + length c > 1
        returnMatch [x] = Just (actOn x)

data RoomObject = RoomObject {   objectName :: String
                                , objectAliases :: [String], 
                                objectReactions :: (Action -> State World [String])} 

instance Show RoomObject where
    show = show . objectName

instance Eq RoomObject where
    (==) r1 r2 = (objectName r1) == (objectName r2)

instance Actionable RoomObject where
    actOn obj action = (reaction action)
        where reaction = objectReactions obj
    match s obj = s `elem` ([objectName obj] ++ objectAliases obj)

getTextForAction :: RoomObject -> Action -> String 
getTextForAction obj act = "Test" 

data Exit = Exit { exitName :: String
                , exitAliases :: [String] 
                , exitDescription :: String
                , exitActions :: (Action -> State World [String])
                , exitOpened :: Bool } 

instance Show Exit where
    show = show . exitName

instance Eq Exit where
    (==) e1 e2 = (exitName e1) == (exitName e2)

instance Actionable Exit where
    actOn exit action = (reaction action)
        where reaction = exitActions exit
    match s ex  = s `elem` ([exitName ex] ++ exitAliases ex)

data Character = Character {    name :: String, 
                                aliases :: [String], 
                                topics :: Conversations, 
                                npcReactions :: Interactions } 
                deriving (Show, Eq)

instance Actionable Character where
    actOn char action = error "Not implemented"
    match s npc = s `elem` ([name npc] ++ aliases npc)

mapFromRooms :: [Room] -> Map.Map String Room
mapFromRooms rs = Map.fromList $ map (\r -> (roomName r, r)) rs

getRoomByName :: World -> String -> Room
getRoomByName w s = case Map.lookup s $ worldRooms w of
                        Nothing -> error "Error in the room names"
                        Just r -> r



basicMove :: Room -> Action -> State World [String]
basicMove r Move = singleAnswer "HERE PUT A MOVE"
basicMove _ _ = singleAnswer "What on earth are you trying to do ?"

singleAnswer :: String -> State World [String]
singleAnswer s = state $ (,) [s]
