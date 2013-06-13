module Lambdacula.World
(
    WorldSituation,
    VertexToNodeInfo,
    KeyToVertex,
    FullGraphInfo,
    Room(..),
    RoomObject(..),
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
    World(..),
    Actionable(..),
    ObjectStatus(..),
    roomByString,
    objectDescription,
    WorldAction,
    worldRooms,
    currentRoom,
    worldObjects,
    playerObjects,
    objectAliases,
    playerPockets,
    identify,
    identifyWithContained,
    canBeNamed
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

type WorldAction = State World [String]
type WorldSituation = State World ()
type VertexToNodeInfo = Vertex -> (Room, String, [String])
type KeyToVertex = String -> Maybe Vertex
type FullGraphInfo = (Graph, VertexToNodeInfo, KeyToVertex)

-- Constant virtual room for inventory
playerPockets = "POCKETS"

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
                 ,_roomstatus :: ObjectStatus
                }
  deriving (Eq)

-- The world. A scary place. It figures the encounters between a hero,
-- the Player, and one of many room (the rooms) in a CurrentRoom.
data World = World { _currentRoom :: Room, 
                    _previousRoom :: Room,
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
                    Nothing -> error $ "Room" ++ s ++ " does not exist ! Fatal error and all that jazz."

-- Player lenses
playerObjects :: Getter World [RoomObject]
playerObjects = to (\w -> filter (isInRoom playerPockets) (_worldObjects w))

newtype ObjectNames = ObjectNames{ names :: [String] }

type RoomObjectBehaviour = RoomObject -> Action -> Maybe String -> WorldAction

data ObjectStatus = Opened | Closed | Broken | Fixed | Hidden | Dark | Nada
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
    (==) r1 r2 = mainName r1 == mainName r2 && _inRoom r1 == _inRoom r2

instance Actionable RoomObject where
    actOn r = view objectReactions r r
    match s = (s `elem`) . map(map toLower) . view objectAliases

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

-- Identify objects corresponding to a name
-- In the given context, namely : among the objects in the room and
-- the objects in the player inventory.
identify :: String -> World -> [RoomObject]
identify s w = identifyObjectWithName s contextObjects 
    where
        contextObjects = (w^.currentObjects) ++ (w^.playerObjects)

-- Identify objects with the given context
-- BUT also objects contained in other objects
-- in the current room. 
identifyWithContained :: String -> World -> [RoomObject]
identifyWithContained s w = identifyObjectWithName s fullContext 
    where
        containerAndContained ros = [ro| ro <- ros, ro <- ro^.containedObjects]
        fullContext =  (containerAndContained (w^.currentObjects)) ++ (w^.playerObjects)

identifyObjectWithName :: String -> [RoomObject] -> [RoomObject]
identifyObjectWithName s = filter (canBeNamed s) 

canBeNamed :: String -> RoomObject -> Bool
canBeNamed s ro = s `elem` (ro^.objectAliases)

