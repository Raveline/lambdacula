module Lambdacula.ModelShortcuts
(
    openContainer,
    pickItemFromContainer,
    changeStatus, 
    changeRoom,
    openDoor,
    pickItem
) 
where

import Lambdacula.World
import Lambdacula.Display

import Control.Lens hiding (contains)
import Control.Monad.State
import Data.List

--------------------
-- Abstract utilities
--------------------

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

