module Lambdacula.World
(
    Room(..),
    objects,
    RoomObject(..),
    objectStatus,
    findObjectInteraction,
    Character(..),
    Exit(..),
    exitName,
    exitDescription,
    exitActions,
    Player(..),
    World(..),
    Actionable(..),
    ObjectStatus(..),
    singleAnswer,
    WorldAction,
    worldRooms,
    currentRoom,
    updateCurrentRoom,
    updateRoomObjects,
    updateObjectStatus,
    pickItem
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
                 ,_characters :: [Character]
                 ,_exits :: [Exit]
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
inventory :: Simple Lens Player [String]
inventory = lens _inventory (\p inv -> p {_inventory = inv})

-- A room object. A thing that stays in a room, waiting to be acted upon,
-- or acting as a simple decoy.
-- It has a main name for identification, and potential aliases for one same
-- thing tends to have different names.
-- It stores the way its react to actions through a method.
-- It can contains other objects.
-- It finally can have a Status. Changing objects status is more or less the
-- way to navigate through the game.
data RoomObject =    RoomObject { 
                     _objectName :: String
                    ,_objectAliases :: [String]
                    ,_objectReactions :: RoomObject -> Action -> WorldAction
                    ,_objectInside :: [RoomObject]
                    ,_objectStatus :: ObjectStatus
                    }

-- RoomObject lenses
objectStatus :: Simple Lens RoomObject ObjectStatus
objectStatus = lens _objectStatus (\ro s -> ro {_objectStatus = s})

objectName :: Simple Lens RoomObject String
objectName = lens _objectName (\ro s -> ro {_objectName = s}) 

objectAliases :: Simple Lens RoomObject [String]
objectAliases = lens _objectAliases (\ro als -> ro {_objectAliases = als})

objectReactions :: Simple Lens RoomObject (RoomObject -> Action -> WorldAction)
objectReactions = lens _objectReactions (\ro rea -> ro {_objectReactions = rea}) 

-- Room object instances
instance Show RoomObject where
    show = show . view objectName

instance Eq RoomObject where
    (==) r1 r2 = view objectName r1 == view objectName r2

instance Actionable RoomObject where
    actOn r = view objectReactions r r
    match s = (s `elem`) . ((:) <$> view objectName <*> view objectAliases)

-- An exit. Actually, currently, I'm really wondering if this particular
-- type is necessary. For an exit is a RoomObject, if you think about it.
-- I might want to make another constructor for RoomObject just for this.
data Exit = Exit { _exitName :: String
                , _exitAliases :: [String] 
                , _exitDescription :: String
                , _exitActions :: Exit -> Action -> WorldAction 
                , _exitOpened :: Bool } 

exitActions :: Simple Lens Exit (Exit -> Action -> WorldAction)
exitActions = lens _exitActions (\ex rea -> Exit (_exitName ex) (_exitAliases ex) (_exitDescription ex) rea (_exitOpened ex))

exitName :: Simple Lens Exit String
exitName = lens _exitName (\ex name -> Exit name (_exitAliases ex) (_exitDescription ex) (_exitActions ex) (_exitOpened ex))

exitDescription :: Simple Lens Exit String
exitDescription = lens _exitDescription (\ex desc -> Exit (_exitName ex) (_exitAliases ex) desc (_exitActions ex) (_exitOpened ex))

exitAliases :: Simple Lens Exit [String]
exitAliases = lens _exitAliases (\ex al -> Exit (_exitName ex) al (_exitDescription ex) (_exitActions ex) (_exitOpened ex))

instance Show Exit where
    show e = _exitName e ++ " : " ++ _exitDescription e

instance Eq Exit where
    (==) e1 e2 = e1^.exitName == e2^.exitName

instance Actionable Exit where
    actOn e = (view exitActions e) e 
    match s = (s `elem`) . ((:) <$> _exitName <*> _exitAliases)

-- A character. Someone that should be able to converse with a player. One day.
-- I've got the same issue with characters and exits : shouldn't they be
-- RoomObject, really ?
data Character = Character {    name :: String, 
                                aliases :: [String], 
                                topics :: Conversations, 
                                npcReactions :: Interactions } 
                deriving (Show, Eq)

instance Actionable Character where
    actOn char action = error "Not implemented"
    match s = (s `elem`) . ((:) <$> name <*> aliases)

data ObjectStatus = Opened | Closed | Nada


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
                newWorlds = actionedObjects ++ actionedCharacters ++ actionedExits
                actionedObjects = actOnAll . findAMatch $ _objects room
                actionedCharacters = actOnAll . findAMatch $ _characters room
                actionedExits = actOnAll . findAMatch $ _exits room
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

-- Add an item to the player inventory. 
-- Note that the item thusly taken must be "erased" from where it
-- was taken by the caller. 
pickItem :: World -> RoomObject -> Player 
pickItem world ro = player' & inventory .~ (_objectName ro:(player'^.inventory))
                where player' = _player world

removeFromRoom :: RoomObject -> WorldAction
removeFromRoom ro = do
                        w <- get
                        current <- use currentRoom
                        let newRoom = current & objects .~ filter (== ro) (current^.objects)
                        put (updateCurrentRoom w newRoom)
                        return []
