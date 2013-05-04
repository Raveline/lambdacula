module Lambdacula.World
(
    Room(..),
    objects,
    RoomObject(..),
    inventory,
    containedObjects,
    mainName,
    headName,
    objectStatus,
    findObjectInteraction,
    ObjectNames (..),
    RoomObjectDetails(..),
    RoomObjectBehaviour,
    Player(..),
    World(..),
    Actionable(..),
    ObjectStatus(..),
    objectDescription,
    singleAnswer,
    WorldAction,
    worldRooms,
    currentRoom,
    updateCurrentRoom,
    updateRoomObjects,
    updateObjectStatus,
    pickItem,
    isOpened
)
where

import qualified Data.Map as Map
import Lambdacula.Action
import Data.Char
import Data.List
import Data.Maybe
import Control.Monad.State
import Control.Applicative
import Control.Lens hiding (Action)


type Conversations = Map.Map String String
type Interactions = Map.Map Action String
type WorldAction = State World [String]

-- *************
-- HERE BE TYPES
-- *************

class Actionable f where 
    -- Process an action on this object, return the world 
    -- and a string to tell what happened
    actOn :: f -> Action -> WorldAction 
    -- Check if a string match this object (usually by looking
    -- at aliases).
    match :: String -> f -> Bool

-- A room. A piece of the universe if the universe was only carved
-- out of rooms. Which would be handy for creators of zork-like games.
data Room =    Room { _roomName :: String
                 ,_description :: String
                 ,_objects :: [RoomObject]
                }
  deriving (Show, Eq)

-- Room lenses
objects :: Simple Lens Room [RoomObject]
objects = lens _objects (\r objs ->  r {_objects = objs})

-- The world. A scary place. It figures the encounters between a hero,
-- the Player, and one of many room (the rooms) in a CurrentRoom.
data World = World { _player :: Player, 
                    _currentRoom :: Room, 
                    _worldRooms :: [Room] }

-- World lenses
worldRooms :: Simple Lens World [Room]
worldRooms = lens _worldRooms (\w rs -> w {_worldRooms = rs})
currentRoom :: Simple Lens World Room
currentRoom = lens _currentRoom (\w cr -> w {_currentRoom = cr})

-- A player. A hero. Currently, only there has a quite capitalistic
-- representation of an inventory.
data Player = Player { _inventory :: [String] }
    deriving (Show)

-- Player lenses
inventory :: Simple Lens World [String]
inventory = lens (_inventory . _player) (\w inv -> w {_player = (_player w) {_inventory = inv}})

newtype ObjectNames = ObjectNames{ names :: [String] }

type RoomObjectBehaviour = RoomObject -> Action -> WorldAction

data ObjectStatus = Opened | Closed | Broken | Fixed | Nada
    deriving (Eq)

-- Details of a room object : its current status and its
-- eventual content
data RoomObjectDetails = RoomObjectDetails { _status :: ObjectStatus
                                            ,_objectDescription :: String
                                            , _content :: [RoomObject] }

data RoomObject = Exit { _ronames :: ObjectNames
                        , _robehaviour :: RoomObjectBehaviour
                        , _rodetails :: RoomObjectDetails }
                | RoomObject { _ronames :: ObjectNames
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

objectReactions :: Simple Lens RoomObject (RoomObject -> Action -> WorldAction)
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
findObjectInteraction :: String -> Room -> Maybe (Action -> WorldAction) 
findObjectInteraction s room = case newWorlds of
                                [x] -> Just x
                                _ -> Nothing
            where 
                newWorlds = actionedObjects
                actionedObjects = actOnAll . findAMatch $ _objects room
                findAMatch :: (Actionable a) => [a] -> [a]
                findAMatch = filter (match s) 
                actOnAll :: (Actionable a) => [a] -> [Action -> WorldAction] 
                actOnAll = fmap actOn

-- Given a string, will return the World "as it is" and the string.
singleAnswer :: String -> WorldAction 
singleAnswer = return . (:[])

-- Will replace an object by its newer version in a room
-- Kind of a setter, if you want.
updateRoomObjects :: Room -> RoomObject -> RoomObject -> Room
updateRoomObjects r old new = r & objects .~ rebuildList (_objects r) old new

updateCurrentRoom :: World -> Room -> World
updateCurrentRoom w r = World (_player w) r (rebuildList (view worldRooms w) (view currentRoom w) r)

updateObjectStatus :: RoomObject -> ObjectStatus -> RoomObject
updateObjectStatus ro st = set objectStatus st ro

-- Given a list of items, replace any version of an item by a new one
rebuildList :: (Eq a) => [a] -> a -> a -> [a]
rebuildList [] _ _ = []
rebuildList (x:xs) old new
    | x == old = new:(rebuildList xs old new)
    | otherwise = x:(rebuildList xs old new)  


-- Called when a player wants to pick up an item.
-- We have to add the name of this item to the player's inventory.
-- And we have to remove this item from the room.
pickItem :: RoomObject -> WorldAction
pickItem ro = do
                w <- get
                inventory .= (mainName ro:(w^.inventory))
                removeFromRoom ro
                return ["You picked up " ++ (mainName ro)]

-- Remove an item from the current room because it has been used
removeFromRoom :: RoomObject -> WorldAction
removeFromRoom ro = do
                        w <- get
                        current <- use currentRoom
                        let newRoom = current & objects .~ filter (/= ro) (current^.objects)
                        put (updateCurrentRoom w newRoom)
                        return []

-- Object utilities

isOpened :: RoomObject -> Bool
isOpened ro = ro^.objectStatus == Opened
