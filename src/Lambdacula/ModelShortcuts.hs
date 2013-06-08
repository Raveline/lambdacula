module Lambdacula.ModelShortcuts
(
    MoveAction,
    openContainer,
    pickItemFromContainer,
    changeStatus, 
    changeRoom,
    openDoor,
    pickItem,
    basicMove,
    moveDoor,
    setExternalStatus
) 
where

import Lambdacula.World
import Lambdacula.Display
import Lambdacula.Action

import Control.Lens hiding (contains, Action)
import Control.Monad.State
import Data.List


type MoveAction = String -> RoomObject -> Action -> Maybe String -> State World [String]

-----------------------
-- Abstract utilities
-----------------------

-- Given a list of items, replace any version of an item by a new one
rebuildList :: (Eq a) => [a]    -- A list
                        -> a    -- The old element
                        -> a    -- The new element
                        -> [a]  -- A list with the element replaced
rebuildList xs old new = case find (==old) xs of
                            Just _  -> rebuildList' xs old new
                            Nothing -> (new:xs)
    where
        rebuildList' [] _ _ = []
        rebuildList' (x:xs) old new
            | x == old = new:(rebuildList' xs old new)
            | otherwise = x:(rebuildList' xs old new)

-- Given a simple string, look for potential aliases in the list
removeObjectFromList :: [RoomObject] -> String -> [RoomObject]
removeObjectFromList ros s = case (findObjectToRemove s ros) of
                                Just x -> filter (/= x) ros
                                Nothing -> ros
    where
        findObjectToRemove :: String -> [RoomObject] -> Maybe RoomObject
        findObjectToRemove s = find (elem s . view objectAliases)

---------------------
-- Changing states
---------------------

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

-- Look inside a container and display,
-- if the container is opened, its content.
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
    | ro `contains` s && isOpened ro = do
                            let newRo = ro & containedObjects .~ (removeObjectFromList (ro^.containedObjects) s)
                            w <- get
                            inventory .= (s:(w^.inventory))
                            worldObjects .= rebuildList (w^.worldObjects) ro newRo
                            singleAnswer $ "You picked up " ++ s
    | not (isOpened ro) = singleAnswer $ mainName ro ++ " is not opened !"
    | otherwise = singleAnswer $ "There is no " ++  s ++ " in " ++ (mainName ro) ++ "."

contains :: RoomObject  -- Container
            -> String   -- Object name
            -> Bool
contains ro s = s `elem` (map mainName $ _content . _rodetails $ ro)

openDoor :: String -> RoomObject -> WorldAction
openDoor kName door 
    | not $ isOpened door = do
                            w <- get
                            if possessKey kName w
                                then
                                    do
                                        changeStatus door Opened
                                        return ["You open the door"]
                                else
                                    return ["You don't have this object !"]
    | otherwise = singleAnswer "The door is already opened !"
    where 
        possessKey :: String -> World -> Bool 
        possessKey s w = s `elem` (w^.inventory)  

-------------------------
-- Inventory management
-------------------------

-- Called when a player wants to pick up an item.
-- We have to add the name of this item to the player's inventory.
-- And we have to remove this item from the room.
pickItem :: RoomObject -> WorldAction
pickItem ro = do
                w <- get
                inventory .= (mainName ro:(w^.inventory))
                worldObjects .= filter(/= ro) (w & view worldObjects)
                return ["You picked up " ++ (mainName ro)]

------------
-- Doors
------------

basicMove :: MoveAction
basicMove r passage Move _ 
    | passage^.objectStatus == Opened = do
                    w <- get
                    currentRoom .= roomByString w r 
                    displayCurrentRoom 
    | otherwise = singleAnswer "You can't, the path is closed !"
basicMove _ _ _ _ = singleAnswer "What on earth are you trying to do ?"


-- Handle any move action related to a door.
-- Used to build door objects.
moveDoor :: Maybe String        -- If locked, the key name.
            -> MoveAction       -- A classical move.
-- Player is trying to go through the door
moveDoor key dest door Move _   = goThroughDoor key dest door 
-- Player is trying to use a key on the door.
moveDoor (Just key) dest door Use (Just keyName)
    | door^.objectStatus == Opened = singleAnswer "It's already unlocked."
    | key == keyName = do
                        changeStatus door Opened
                        -- TODO : remove the key
                        return ["You unlock the door."]
    | otherwise = singleAnswer "The key doesn't fit !"
-- We're going to assume this is a move
moveDoor key dest door Use _ = goThroughDoor key dest door
-- Player is trying to open the door.
moveDoor (Just key) dest door Open Nothing = singleAnswer "This door is locked."
moveDoor _ dest door Open Nothing = do
                        changeStatus door Opened
                        return ["You open the door."]
                        
goThroughDoor :: Maybe String   -- Maybe a key
              -> String         -- Destination
              -> RoomObject     -- The door
              -> State World [String]
goThroughDoor key dest door 
    | door^.objectStatus == Closed = singleAnswer $ lockedOrClosed key door
    | otherwise = basicMove dest door Move Nothing 
    where
            lockedOrClosed :: Maybe String -> RoomObject -> String
            lockedOrClosed Nothing ro = present ro ++ " door is closed !"
            lockedOrClosed _ ro = present ro ++ " door is locked !" 

--------------------------
-- Indirect interactions
--------------------------

-- Will change the status of an object, given its name,
-- in a given room.
setExternalStatus :: String             -- Room name
                    -> String           -- Object name
                    -> ObjectStatus     -- New status
                    -> WorldSituation
setExternalStatus roName obName stat = do
                                    w <- get
                                    case findObject w of
                                        Just o -> changeStatus o stat 
                                        Nothing -> return ()
        where
            matcher :: String -> String -> RoomObject -> Bool
            matcher room name ro = (_inRoom ro) == room && (name `elem` (ro^.objectAliases))
            findObject w = find (matcher roName obName) (w^.worldObjects)


    
