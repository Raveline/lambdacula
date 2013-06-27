module Lambdacula.ModelShortcuts
(
    MoveAction,
    numberOfContained,
    openContainer,
    lookInsideContainer,
    pickItemFromContainer,
    putInsideContainer,
    containsSomethingNamed,
    changeStatus, 
    changeRoom,
    openDoor,
    pickItem,
    basicMove,
    flee,
    moveDoor,
    setExternalStatus,
    ifContainsDo
) 
where

import Lambdacula.World
import Lambdacula.Display
import Lambdacula.Action

import Control.Lens hiding (contains, Action)
import Control.Monad.State
import Data.List
import qualified Data.Map as Map

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

----------------------
-- Object utilities --
----------------------

-- Return the number of objects contained in a roomObject
numberOfContained :: RoomObject -> Int
numberOfContained = length . view containedObjects 


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

------------------------
-- Ambiguity processing
------------------------


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
    | otherwise = do return ("It contains : ":displayContainerContent ro)
    where
        displayContainerContent ro = [mainName x| x <- (ro^.containedObjects)]

putInsideContainer :: RoomObject    -- The container
                        -> String   -- The object to put
                        -> String   -- Text if it works
                        -> WorldAction
putInsideContainer container objectName workingText
    | not $ isOpened container = singleAnswer "Errr... it's closed. You should open it first."
    | otherwise = do
                    w <- get
                    case getFromInventory objectName w of
                        Nothing -> singleAnswer "You don't have this object !"
                        Just contained -> do
                            putItemInContainer contained container 
                            return [workingText]

pickItemFromContainer :: RoomObject         -- The container 
                        -> String           -- The object to pick
                        -> WorldAction
pickItemFromContainer container x = do
                                        w <- get
                                        case containeds x w of
                                            []          -> singleAnswer $ "What on earth are you talking about ?"
                                            [object]    -> pickItemFromContainer' container object
                                            (xs)        -> error "Ambiguous case. Not implemented yet."
    where
        containeds x w =  identifyWithContained x w
        pickItemFromContainer' :: RoomObject -> RoomObject -> WorldAction
        pickItemFromContainer' container contained
            | container `contains` contained && isOpened container = do
                            removeItemFromContainer container contained
                            singleAnswer $ "You picked up " ++ (mainName contained)
            | not (isOpened container) = singleAnswer $ mainName container ++ " is not opened !"


-- To remove an item from a container, we must :
-- 1°) Redefine the container as "not containing the contained"
-- 2°) Add the contained object to the list of world's owned RoomObject 
removeItemFromContainer ::  RoomObject          -- Container
                            -> RoomObject       -- Contained
                            -> WorldSituation
removeItemFromContainer container contained = do
                            let newContainer = container & containedObjects .~ (removeObjectFromList (container^.containedObjects) (mainName contained))
                            objects <- use worldObjects
                            worldObjects .= rebuildList objects container newContainer
                            addToInventory contained
                            objects <- use worldObjects
                            worldObjects .= contained:objects
                            return ()

putItemInContainer :: RoomObject
                    -> RoomObject
                    -> WorldSituation
putItemInContainer container contained = do
                        item <- removeFromInventory $ contained
                        let newContainer = container & containedObjects .~ (item:(container^.containedObjects))
                        objects <- use worldObjects
                        worldObjects .= rebuildList objects container newContainer
                        return()

removeFromInventory :: RoomObject
                        -> State World RoomObject 
removeFromInventory item = do
                        let newItem = item { _inRoom = "" } 
                        objects <- use worldObjects
                        worldObjects .= rebuildList objects item newItem
                        return newItem

addToInventory :: RoomObject    -- The object to change
                -> WorldSituation
addToInventory = changeRoom playerPockets 

contains :: RoomObject  -- Container
            -> RoomObject -- Object name
            -> Bool
contains container contained = contained `elem` (_content . _rodetails $ container)

containsSomethingNamed :: RoomObject        -- Container
                        -> String           -- Object name
                        -> Bool
containsSomethingNamed container containedName = containedName `elem` (allNames $ view containedObjects container)
    where
        allNames :: [RoomObject] -> [String]
        allNames = concat . extractNames
        extractNames :: [RoomObject] -> [[String]]
        extractNames = map (view objectAliases)

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

hasInInventory :: String
                -> World
                -> Bool
hasInInventory name world = length (findObjectWithName name (world^.playerObjects)) > 0
    where
        findObjectWithName :: String -> [RoomObject] -> [RoomObject]
        findObjectWithName s ros = filter (canBeNamed s) ros

getFromInventory :: String
                -> World
                -> Maybe RoomObject
getFromInventory name world = find (canBeNamed name) (world^.playerObjects)


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

basicMove :: MoveAction
basicMove r passage Move _ 
    | passage^.objectStatus == Opened = do
                    w <- get
                    current <- use currentRoom
                    previousRoom .= current 
                    currentRoom .= roomByString w r 
                    displayCurrentRoom 
    | otherwise = singleAnswer "You can't, the path is closed !"
basicMove _ _ _ _ = singleAnswer "What on earth are you trying to do ?"

flee :: WorldAction
flee = do
        w <- get
        currentRoom .= (w^.previousRoom)
        displayCurrentRoom


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

