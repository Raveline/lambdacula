module Lambdacula.World
(
    -- Data
    Room(..),
    RoomObject(..),
    ObjectNames (..),
    RoomObjectDetails(..),
    DoorInfo(..),
    World(..),
    ObjectStatus(..),
    Reaction (..),
    Condition (..),
    Interactor (..),
    -- types
    WorldSituation,
    VertexToNodeInfo,
    KeyToVertex,
    FullGraphInfo,
    ActionDetail,
    WorldAction,
    Reactions,
    ReactionSet,
    -- Functions
    containedObjects,
    currentObjects,
    currentRoomName,
    mainName,
    headName,
    objectStatus,
    isInRoom,
    findObjectInteraction,
    roomByString,
    objectDescription,
    worldRooms,
    currentRoom,
    previousRoom,
    worldObjects,
    playerObjects,
    objectAliases,
    playerPockets,
    identify,
    identifyWithContained,
    canBeNamed,
    localScope
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
type ActionDetail = (RoomObject, Action, Maybe Interactor)
type ProcessedAction = Either [String] ActionDetail
type IdentifiedObject = Either [String] RoomObject
type IdentifiedInteractor = Either [String] Interactor
type Reactions = [Reaction]
type ReactionSet = (String, Action, Maybe String, [Condition], Reactions)

-- Constant virtual room for inventory
playerPockets = "POCKETS"

-- *************
-- HERE BE TYPES
-- *************

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
                    _reactions :: [ReactionSet],
                    _getARoom :: String -> Maybe Vertex,
                    _getANode :: Vertex -> (Room, String, [String])}


-- World lenses
worldRooms :: Simple Lens World Graph 
worldRooms = lens _worldRooms (\w rs -> w {_worldRooms = rs})
currentRoom :: Simple Lens World Room
currentRoom = lens _currentRoom (\w cr -> w {_currentRoom = cr})
previousRoom :: Simple Lens World Room
previousRoom = lens _previousRoom (\w cr -> w {_previousRoom = cr})
worldObjects :: Simple Lens World [RoomObject]
worldObjects = lens _worldObjects (\w wos -> w {_worldObjects = wos})
currentRoomName :: Getter World String
currentRoomName = to (\w -> _roomName . _currentRoom $ w)

-- Give the objects belonging to the current room
currentObjects :: Getter World [RoomObject]
currentObjects = to (\w -> filter (isInRoom  (_roomName . _currentRoom $ w)) (_worldObjects w))

-- Give the objects belonging to the current room... AND their contained objects.
fullCurrentObjects :: Getter World [RoomObject]
fullCurrentObjects = to (\w -> [ro|ro <- (view currentObjects w)] ++ [ro| ro <- (view currentObjects w), ro <- ro^.containedObjects])

isInRoom :: String   -- The name of the current room
            -> RoomObject  -- An object to consider
            -> Bool     -- Is the object in this room ?
isInRoom name ro = _inRoom ro == name
            

roomByString :: World -> String -> Room
roomByString w s = case _getARoom w s of
                    Just a -> _getANode w a ^. _1
                    Nothing -> error $ "Room" ++ s ++ " does not exist ! Fatal error and all that jazz."

-- Player lenses
playerObjects :: Getter World [RoomObject]
playerObjects = to $ filter (isInRoom playerPockets) . _worldObjects

newtype ObjectNames = ObjectNames{ names :: [String] }

data ObjectStatus = Opened | Closed | Broken | Fixed | Hidden | Dark | Luminescent | Powered | Salted | Nada
    deriving (Eq, Ord, Show)

-- Details of a room object : its current status and its
-- eventual content
data RoomObjectDetails = RoomObjectDetails { _status :: ObjectStatus
                                            ,_objectDescription :: String
                                            , _content :: [RoomObject] }


data DoorInfo = DoorInfo { key :: Maybe String }

data RoomObject = Exit { _ronames :: ObjectNames            
                        , _inRoom :: String                  
                        , _rodetails :: RoomObjectDetails 
                        , doorInfo :: Maybe DoorInfo
                        , _rodestination :: String }
                | RoomObject { _ronames :: ObjectNames
                            , _inRoom :: String
                            , _rodetails :: RoomObjectDetails}
mainName :: RoomObject -> String
mainName = headName . _ronames
headName :: ObjectNames -> String
headName = head . names

-- RoomObject lenses
objectDetails :: Simple Lens RoomObject RoomObjectDetails
objectDetails = lens _rodetails (\ro d -> ro {_rodetails = d })

objectStatus :: Simple Lens RoomObject ObjectStatus
objectStatus = lens (_status . _rodetails) (\ro s -> ro {_rodetails = (_rodetails ro) {_status = s}})

objectAliases :: Simple Lens RoomObject [String]
objectAliases = lens (names . _ronames ) (\ro als -> ro {_ronames = ObjectNames als})

objectDescription :: Simple Lens RoomObject String
objectDescription = lens (_objectDescription . _rodetails) (\ro od -> ro {_rodetails = (_rodetails ro) {_objectDescription = od}})

containedObjects :: Simple Lens RoomObject [RoomObject]
containedObjects = lens (_content . _rodetails) (\ro ct -> ro {_rodetails = (_rodetails ro) {_content = ct}})

-- Reactions
data Reaction =  Display String                             -- Display some text
                | PickItem String                           -- Add to inventory
                | RemoveItem String                         -- Remove from inventory
                | ChangeStatus String ObjectStatus          -- Change object status
                | PickFromContainer String String           -- Pick from container (1) object (2)
                | GetFromCharacter String String            -- Get from character (1) object (2). Like PickFromContainer, but no control on status.
                | LookInsideContainer String String         -- Look content of container (1) with intro sentence (2)
                | PutInsideContainer String String String   -- Put inside container (1) the item (2) with resulting sentence (3)
                | RebranchTo Action String (Maybe String)   -- Rephrase a command so that it'll be retranslated 
                | Conversation [(String, [String])] [(String, String)] String
                | MoveTo String                             -- Move somewhere
                | RemoveFromWorld String                    -- Remove from the world something
                | Flight                                    -- Flee
                | LookAround                                -- Display current room
                | Error                                     -- Not Implemented Yet 
    deriving (Eq, Show)

-- Room object instances
instance Show RoomObject where
   show = show . mainName

instance Eq RoomObject where
    (==) r1 r2 = mainName r1 == mainName r2 && _inRoom r1 == _inRoom r2


-- Conditions
data Condition = ContainsAmountOfItem (Int -> Bool) -- Does the object contains x items ?
                | PlayerHasStatus ObjectStatus      -- Does the player have a status ?
                | PlayerHasObject String            -- Does the player have an object ?
                | HasStatus ObjectStatus            -- Does the object has a status ?
                | IsThereA String                   -- Is something in scope ?
                | Contains String                   -- Does the object contains X ?

data Interactor = ObjectInteractor RoomObject
                | StringInteractor String

-- Given a string and a room, try to find a RoomObject,
-- an Exit or a Character matching the string. Send back
-- a potential reaction function.
findObjectInteraction :: String             -- A name
                        -> [RoomObject]     -- A list of objects - typically, the local scope
                        -> Action           -- An action to do
                        -> Maybe String     -- A potential other object
                        -> ProcessedAction  -- A potential action or failure message
findObjectInteraction s ros action interactor = 
    packAction mainObject $ handleInteractor action interactor
        where 
        mainObject = wordFilter s ros

        -- In conversation case WE MUST NOT REDUCE THE COMPLEMENT, or aliases will
        -- wreak HAVOC.
        handleInteractor :: Action -> Maybe String -> Maybe IdentifiedInteractor
        handleInteractor _ Nothing = Nothing
        handleInteractor Talk (Just x) = Just (Right $ StringInteractor x)
        handleInteractor _ (Just x) = Just (interactorFilter x ros)

        packAction :: IdentifiedObject -> Maybe IdentifiedInteractor -> ProcessedAction
        packAction (Right ro) Nothing = Right (ro, action, Nothing)
        packAction (Right ro) (Just (Right ro')) = Right (ro, action, Just ro')
        packAction (Left x) _ = Left x
        packAction _ (Just(Left x)) = Left x
        

-- Given a world, and a list of potential objects :
-- Look for the object corresponding to a given name.
-- If no object or more than one object are found, return a Left string.
-- If one and only one object is found, return it as Right.
wordFilter :: String -> [RoomObject] -> IdentifiedObject
wordFilter s ros = case findAMatch s ros of
            [] -> Left ["I don't think you can interact with " ++ s]
            [x] -> Right x
            (xs) -> Left $ ambiguityProcessing s xs

interactorFilter :: String -> [RoomObject] -> IdentifiedInteractor
interactorFilter s ros = case findAMatch s ros of
            [] -> Right $ StringInteractor s
            [x] -> Right $ ObjectInteractor x
            (xs) -> Left $ ambiguityProcessing s xs

findAMatch s = filter (match s) 

ambiguityProcessing :: String -> [RoomObject] -> [String]
ambiguityProcessing s ro = (s ++ " can mean many things. I need you to narrow it down among : "): nameObjects ro
    where
        nameObjects :: [RoomObject] -> [String]
        nameObjects ro = ["- " ++ mainName x|x <- ro]

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
        fullContext =  containerAndContained (w^.currentObjects) ++ (w^.playerObjects)

identifyObjectWithName :: String -> [RoomObject] -> [RoomObject]
identifyObjectWithName s = filter (canBeNamed s) 

canBeNamed :: String -> RoomObject -> Bool
canBeNamed s ro = lowerStr s `elem` (map lowerStr $ ro^.objectAliases)
    where
        lowerStr = map toLower

match = canBeNamed 

-- Give all potential objects the player can interact with
-- Namely, its surroundings and inventory
localScope :: State World [RoomObject]
localScope = do
                objs <- use fullCurrentObjects  -- Visible objects in the world
                inv <- use playerObjects    -- Inventory of the player
                return $ objs ++ inv
