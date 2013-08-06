module Lambdacula.ModelShortcuts
(
    MoveAction,
    numberOfContained,
    openContainer,
    lookInsideContainer,
    pickItemFromContainer,
    putInsideContainer,
    containsSomethingNamed,
    addToInventory,
    removeFromInventoryByName,
    changeStatus, 
    changeRoom,
    openDoor,
    pickItem,
    basicMove,
    flee,
    moveDoor,
    setExternalStatus,
    ifContainsDo,
    accordingToStatus
) 
where

import Lambdacula.World
import Lambdacula.Display
import Lambdacula.Action


type MoveAction = String -> RoomObject -> Action -> Maybe String -> State World [String]

-----------------------
-- Abstract utilities
-----------------------



----------------------
-- Object utilities --
----------------------

-- Return the number of objects contained in a roomObject
numberOfContained :: RoomObject -> Int
numberOfContained = length . view containedObjects 


---------------------
-- Changing states
---------------------


---------------------
-- Container related
---------------------

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

removeFromInventoryByName :: String -> WorldSituation
removeFromInventoryByName s = do
                                w <- get
                                case getFromInventory s w of
                                    Just x -> do 
                                        removeFromInventory x
                                        return ()
                                    Nothing -> error $ "Cannot remove item" ++ s ++ " from inventory !"



openDoor :: String -> RoomObject -> WorldAction
openDoor kName door 
    | not $ isOpened door = do
                            w <- get
                            if hasInInventory kName w
                                then
                                    do
                                        changeStatus door Opened
                                        return ["You open the door"]
                                else
                                    return ["You don't have this object !"]
    | otherwise = singleAnswer "The door is already opened !"




-------------------------
-- Inventory management
-------------------------

-- Called when a player wants to pick up an item.
-- We have to add the name of this item to the player's inventory.
-- And we have to remove this item from the room.
pickItem :: RoomObject -> WorldAction
pickItem ro = do
                w <- get
                addToInventory ro
                return ["You picked up " ++ (mainName ro)]

hasLighting :: World -> Bool
hasLighting w = length lightsources > 0
    where 
        lightsources = filter (isLuminescent) (view playerObjects w)
        isLuminescent :: RoomObject -> Bool
        isLuminescent ro = view objectStatus ro == Luminescent

------------
-- Doors
------------




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

-----------------------
-- Complex behaviour --
-----------------------

ifContainsDo ro actions = case Map.lookup (numberOfContained ro) actions of
                    Just x -> x
                    Nothing -> error $ "No answer for " ++ show (numberOfContained ro) ++ " in " ++ (mainName ro)

accordingToStatus ro actions = case Map.lookup (view objectStatus ro) actions of
                    Just x -> x
                    Nothing -> error $ "No answer for " ++ show (view objectStatus ro) ++ " in " ++ (mainName ro) 
