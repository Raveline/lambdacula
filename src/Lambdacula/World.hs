module Lambdacula.World
(
    WorldSituation,
    VertexToNodeInfo,
    KeyToVertex,
    FullGraphInfo,
    Room(..),
    RoomObject(..),
    inventory,
    containedObjects,
    currentObjects,
    currentRoomName,
    mainName,
    headName,
    objectStatus,
    isInRoom,
    findObjectInteraction,
    ObjectNames (..),
    RoomObjectDetails(..),
    RoomObjectBehaviour,
    Player(..),
    World(..),
    Actionable(..),
    ObjectStatus(..),
    roomByString,
    objectDescription,
    singleAnswer,
    WorldAction,
    worldRooms,
    currentRoom,
    changeStatus, 
    changeRoom,
    changeName,
    pickItem,
    openContainer
)
where

import qualified Data.Map as Map
import Lambdacula.Action
import Data.Char
import Data.List
import Data.Maybe
import Data.Graph
import Control.Monad.State
import Control.Applicative
import Control.Lens hiding (Action)


type Conversations = Map.Map String String
type Interactions = Map.Map Action String
type WorldAction = State World [String]
type WorldSituation = State World ()
type VertexToNodeInfo = Vertex -> (Room, String, [String])
type KeyToVertex = String -> Maybe Vertex
type FullGraphInfo = (Graph, VertexToNodeInfo, KeyToVertex)

-- *************
-- HERE BE TYPES
-- *************

class Actionable f where 
    -- Process an action on this object, return the world 
    -- and a string to tell what happened
    actOn :: f -> Action -> Maybe String -> WorldAction 
    -- Check if a string match this object (usually by looking
    -- at aliases).
    match :: String -> f -> Bool

-- A room. A piece of the universe if the universe was only carved
-- out of rooms. Which would be handy for creators of zork-like games.
data Room =    Room { _roomName :: String
                 ,_description :: String
                }
  deriving (Show, Eq)

-- The world. A scary place. It figures the encounters between a hero,
-- the Player, and one of many room (the rooms) in a CurrentRoom.
data World = World { _player :: Player, 
                    _currentRoom :: Room, 
                    _worldRooms :: Graph,
                    _worldObjects :: [RoomObject],
                    _getARoom :: (String -> Maybe Vertex),
                    _getANode :: (Vertex -> (Room, String, [String]))}


-- World lenses
worldRooms :: Simple Lens World Graph 
worldRooms = lens _worldRooms (\w rs -> w {_worldRooms = rs})
currentRoom :: Simple Lens World Room
currentRoom = lens _currentRoom (\w cr -> w {_currentRoom = cr})
worldObjects :: Simple Lens World [RoomObject]
worldObjects = lens _worldObjects (\w wos -> w {_worldObjects = wos})
currentRoomName :: Getter World String
currentRoomName = to (\w -> _roomName . _currentRoom $ w)

-- Give the objects belonging to the current room
currentObjects :: Getter World [RoomObject]
currentObjects = to (\w -> filter (isInRoom  (_roomName . _currentRoom $ w)) (_worldObjects w))

isInRoom :: String   -- The name of the current room
            -> RoomObject  -- An object to consider
            -> Bool     -- Is the object in this room ?
isInRoom name ro = (_inRoom ro) == name
            

roomByString :: World -> String -> Room
roomByString w s = case ((_getARoom w) s) of
                    Just a -> ((_getANode w) a)^._1
                    Nothing -> error "THIS IS NOT HAPPENING OH GOD"

-- A player. A hero. Currently, only there has a quite capitalistic
-- representation of an inventory.
data Player = Player { _inventory :: [String] }
    deriving (Show)

-- Player lenses
inventory :: Simple Lens World [String]
inventory = lens (_inventory . _player) (\w inv -> w {_player = (_player w) {_inventory = inv}})

newtype ObjectNames = ObjectNames{ names :: [String] }

type RoomObjectBehaviour = RoomObject -> Action -> Maybe String -> WorldAction

data ObjectStatus = Opened | Closed | Broken | Fixed | Nada
    deriving (Eq)

-- Details of a room object : its current status and its
-- eventual content
data RoomObjectDetails = RoomObjectDetails { _status :: ObjectStatus
                                            ,_objectDescription :: String
                                            , _content :: [RoomObject] }

data RoomObject = Exit { _ronames :: ObjectNames            
                        , _inRoom :: String                  
                        , _robehaviour :: RoomObjectBehaviour
                        , _rodetails :: RoomObjectDetails 
                        , _rodestination :: String }
                | RoomObject { _ronames :: ObjectNames
                            , _inRoom :: String
                            , _robehaviour :: RoomObjectBehaviour
                            , _rodetails :: RoomObjectDetails}
mainName :: RoomObject -> String
mainName = headName . _ronames
headName :: ObjectNames -> String
headName = head . names

-- RoomObject lenses
objectDetails :: Simple Lens RoomObject RoomObjectDetails
objectDetails = lens (_rodetails) (\ro d -> ro {_rodetails = d })

objectStatus :: Simple Lens RoomObject ObjectStatus
objectStatus = lens (_status . _rodetails) (\ro s -> ro {_rodetails = ((_rodetails ro) {_status = s})})

objectAliases :: Simple Lens RoomObject [String]
objectAliases = lens (names . _ronames ) (\ro als -> ro {_ronames = (ObjectNames als)})

objectReactions :: Simple Lens RoomObject RoomObjectBehaviour
objectReactions = lens _robehaviour (\ro rea -> ro {_robehaviour = rea}) 

objectDescription :: Simple Lens RoomObject String
objectDescription = lens (_objectDescription . _rodetails) (\ro od -> ro {_rodetails = ((_rodetails ro) {_objectDescription = od})})

containedObjects :: Simple Lens RoomObject [RoomObject]
containedObjects = lens (_content . _rodetails) (\ro ct -> ro {_rodetails = ((_rodetails ro) {_content = ct})})

-- Room object instances
instance Show RoomObject where
   show = show . mainName

instance Eq RoomObject where
    (==) r1 r2 = mainName r1 == mainName r2

instance Actionable RoomObject where
    actOn r = view objectReactions r r
    match s = (s `elem`) . ((:) <$> mainName <*> view objectAliases)

-- *****************
-- UTILITY FUNCTIONS
-- *****************

-- Given a string and a room, try to find a RoomObject,
-- an Exit or a Character matching the string. Send back
-- a potential reaction function.
findObjectInteraction :: String                             -- A name
                        -> [RoomObject]                     -- A list of objects
                        -> Maybe (Action -> Maybe String -> WorldAction)    -- A potential action
findObjectInteraction s ros = case newWorlds of
                                [x] -> Just x
                                _ -> Nothing
            where 
                newWorlds = actionedObjects
                actionedObjects = actOnAll . findAMatch $ ros
                findAMatch :: (Actionable a) => [a] -> [a]
                findAMatch = filter (match s) 
                actOnAll :: (Actionable a) => [a] -> [Action -> Maybe String -> WorldAction] 
                actOnAll = fmap actOn

-- Given a string, will return the World "as it is" and the string.
singleAnswer :: String -> WorldAction 
singleAnswer = return . (:[])

-- Change the status of an object
changeStatus :: RoomObject          -- The room object to change
                -> ObjectStatus     -- The new status
                -> WorldSituation   -- Return a state World ()
changeStatus ro st = do
                        wos <- use worldObjects
                        worldObjects .= rebuildList wos ro (ro & objectStatus.~ st)
                        return ()
-- Change the room an object is stored in.
changeRoom :: String
            -> RoomObject        -- The room object to change
            -> WorldSituation   -- Return a state World ()
changeRoom name ro = do
                        wos <- use worldObjects
                        worldObjects .= rebuildList wos ro (ro { _inRoom = name })
                        return ()

changeName :: String
            -> RoomObject
            -> WorldSituation
changeName name ro = do
                        wos <- use worldObjects
                        worldObjects .= rebuildList wos ro (ro { _ronames = (ObjectNames ["Test"])}) 
                        return ()

-- Given a list of items, replace any version of an item by a new one
rebuildList :: (Eq a) => [a]    -- A list
                        -> a    -- The old element
                        -> a    -- The new element
                        -> [a]  -- A list with the element replaced
rebuildList xs old new = case find (==old) xs of
                            Just _  -> rebuildList' xs old new
                            Nothing -> (new:xs)
rebuildList' [] _ _ = []
rebuildList' (x:xs) old new
    | x == old = new:(rebuildList' xs old new)
    | otherwise = x:(rebuildList' xs old new)


-- Called when a player wants to pick up an item.
-- We have to add the name of this item to the player's inventory.
-- And we have to remove this item from the room.
pickItem :: RoomObject -> WorldAction
pickItem ro = do
                w <- get
                inventory .= (mainName ro:(w^.inventory))
                worldObjects .= filter(/= ro) (w & view worldObjects)
                return ["You picked up " ++ (mainName ro)]

---------------------
-- Container related
---------------------

-- Object utilities
isOpened :: RoomObject -> Bool
isOpened ro = ro^.objectStatus == Opened

-- Given a room object, and a success string, open the container if possible
-- and display its content
openContainer :: RoomObject -> String -> WorldAction
openContainer ro sust
    | isOpened ro = singleAnswer "It's already opened !"
    | otherwise = do
                    w <- get
                    changeStatus ro Opened
                    name <- use currentRoomName
                    lookInsideContainer ro

lookInsideContainer :: RoomObject -> WorldAction
lookInsideContainer ro
    | isOpened ro = singleAnswer $ (headName . _ronames $ ro) ++ " is closed, you can't look inside."
    | otherwise = do 
                    return ("It contains : ":displayContainerContent ro)
    where
        displayContainerContent ro = [mainName x| x <- (ro^.containedObjects)]

pickItemFromContainer :: RoomObject         -- The container 
                        -> String           -- The object to pick
                        -> WorldAction
pickItemFromContainer ro s
    | s `elem` (map mainName $ _content . _rodetails $ ro) = do
                                                let newRo = ro & containedObjects .~ (removeObjectFromList (ro^.containedObjects) s)
                                                w <- get
                                                worldObjects .= rebuildList (w^.worldObjects) ro newRo
                                                singleAnswer $ "You picked up " ++ s
    | otherwise = singleAnswer $ "There is no " ++  s ++ " in " ++ (mainName ro) ++ "."

-- Given a simple string, look for potential aliases in the list
removeObjectFromList :: [RoomObject] -> String -> [RoomObject]
removeObjectFromList ros s = case (findObjectToRemove s ros) of
                                Just x -> filter (/= x) ros
                                Nothing -> ros
    where
        findObjectToRemove :: String -> [RoomObject] -> Maybe RoomObject
        findObjectToRemove s = find (elem s . view objectAliases)
