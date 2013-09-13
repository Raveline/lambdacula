-- Please note. Reactions use only String alias.
-- This string will be "converted" to the proper, "real" roomObject.
-- Which means you should only use UNIQUE string for reactions.
-- Runtime error will happen if you don't.

module Lambdacula.Reactions
( 
    Reactions,
    ReactionSet,
    processAction,
    onlyDo,
    onlyDisplay,
    findReactions
)
where

import Lambdacula.World
import Lambdacula.Display
import Lambdacula.Action

import Control.Lens hiding (contains, Action)
import Control.Monad.State
import qualified Data.Foldable as Fd
import Data.List
import qualified Data.Map as Map

type Feedback = Maybe [String]
type WorldFeedback = State World Feedback
type PureActionDetail = (String, Action, Maybe String)
type Aliases = [(String, [String])]
type Topic = [(String, String)]

onlyDisplay :: String -> WorldAction
onlyDisplay s = onlyDo $ Display s

onlyDo :: Reaction -> WorldAction
onlyDo r = do
                feedback <- processReaction r
                case feedback of 
                    Just s' -> return s'
                    Nothing -> return []
                    

processAction :: (RoomObject, Action, Maybe RoomObject) -> WorldAction
-- Doors & exit interactions
processAction ((Exit name _ (RoomObjectDetails status _ _) info dest), a, x) = handleExit (headName name) info status dest a x
processAction (x, a, Just (Exit name _ (RoomObjectDetails status _ _) info dest)) = handleExit (headName name) info status dest a (Just x)
-- Other interactions
processAction detail = do
                    reactions <- findReactions $ simplifyAction detail
                    processReactions reactions

processReactions :: [Reaction] -> State World [String]
processReactions = Fd.foldlM process []  

process :: [String] -> Reaction -> State World [String]
process strs reac = do
    feedback <- processReaction reac 
    return $ extractStr strs feedback
    where
        extractStr :: [String] -> Feedback -> [String]
        extractStr strs (Just s) = s ++ strs
        extractStr strs Nothing = strs 

handleExit :: String                -- Name of the Exit
            -> Maybe DoorInfo          -- If it's a door, information on it
            -> ObjectStatus         -- Current status
            -> String               -- Destination name
            -> Action               -- Action taken
            -> Maybe RoomObject     -- Potential interaction, with a key normally
            -> WorldAction          -- Returns a world and an output 
-- OPENING
handleExit n (Just di) Closed _ Use (Just k')
    | testKey (key di) (mainName k') = do
                                        keyContained <- inventoryContains k'
                                        if keyContained
                                        then processReactions [ChangeStatus n Opened]
                                        else return ["You don't have this key on you !"]
    | otherwise = return ["This is the wrong key."]
handleExit n Nothing Closed _ Open _ = processReactions [ChangeStatus n Opened]
-- MOVES : impossible
handleExit _ (Just (DoorInfo (Just di))) Closed _ Move _ = return ["The door is locked !"]
handleExit _ (Just (DoorInfo Nothing)) Closed _ Move _ = return ["The door is closed !"]
-- MOVES : possible
handleExit _ _ Opened s Move _ = processReactions [MoveTo s]

testKey :: Maybe String -> String -> Bool
testKey Nothing _ = False
testKey (Just k) k' = k == k'

findReactions :: PureActionDetail -> State World [Reaction]
findReactions specs = do
            w <- get
            scope <- localScope
            case (find (satisfy w scope specs) (_reactions w)) of
                Just px -> return $ extractReactions px
                Nothing -> return []
    where
        satisfy :: World -> [RoomObject] -> PureActionDetail -> ReactionSet -> Bool
        satisfy w scope set1 set2 = matchAction set1 set2 && testConditions (extractCondition set2) w (realObject set1 scope)
        matchAction :: PureActionDetail -> ReactionSet -> Bool
        matchAction (objA, action, objB) (objA', action', objB', _, _) = objA == objA' && action == action' && objB == objB'
        realObject :: PureActionDetail -> [RoomObject] -> RoomObject
        realObject (n, _, _) scope = fetchByNameInScope n scope 
        extractCondition :: ReactionSet -> [Condition]
        extractCondition (_,_,_,cs,_) = cs
        testConditions :: [Condition] -> World -> RoomObject -> Bool
        testConditions cs w r = all (== True) $ map (testCondition w r) cs
        extractReactions :: ReactionSet -> Reactions
        extractReactions rs = view _5 rs

simplifyAction :: ActionDetail -> PureActionDetail
simplifyAction (ro, a, Nothing) = (mainName ro, a, Nothing)
simplifyAction (ro, a, (Just ro')) = (mainName ro, a, Just(mainName ro'))

processReaction :: Reaction -> WorldFeedback
processReaction (Display s) = singleAnswer s
processReaction (ChangeStatus objName status) = do 
                        obj <- fetchByName objName
                        changeStatus obj status
processReaction (PickFromContainer container contained) = do
                        objContainer <- fetchByName container
                        pickItemFromContainer objContainer contained
processReaction (LookInsideContainer contName intro) = do
                        obj <- fetchByName contName
                        lookInsideContainer obj intro
processReaction (PutInsideContainer container item resultSentence) = do
                        objContainer <- fetchByName container
                        putInsideContainer objContainer item resultSentence
processReaction (RebranchTo act n react) = error "NIY"
processReaction (Conversation aliases topics) = handleConversation aliases topics
processReaction (LookAround) = displayCurrentRoom
processReaction (MoveTo location) = basicMove location


testCondition :: World -> RoomObject -> Condition -> Bool
testCondition w r (ContainsAmountOfItem x) = (==) x . length . view containedObjects $ r
testCondition w r (PlayerHasStatus stat) = error "NIY"
testCondition w r (HasStatus stat) = (==) stat . view objectStatus $ r
testCondition w ro (Contains name) = ro `containsSomethingNamed` name  

------------------------------------------------
-- All utilies methods should be stored there --
------------------------------------------------

noReaction :: State World [String]
noReaction = return []

-- Code utils
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

-- SHORTCUTS 
isOpened :: RoomObject -> Bool
isOpened ro = view objectStatus ro == Opened

fetchByName :: String -> State World RoomObject 
fetchByName s = do
                scope <- localScope
                return $ fetchByNameInScope s scope 
fetchByNameInScope :: String -> [RoomObject] -> RoomObject
fetchByNameInScope s ros = case finder ros of
                    Nothing -> error $ "Object " ++ s ++ " not found in scope : " ++ (show ros) ++ ". This should not happen."
                    Just x -> x
    where
        finder = find (\ro -> (mainName ro) == s)

-- CONTAINER UTILITIES

-- Check that a container contains something with a given name
containsSomethingNamed :: RoomObject        -- Container
                        -> String           -- Object name
                        -> Bool
containsSomethingNamed container containedName = containedName `elem` (allNames $ view containedObjects container)
    where
        allNames :: [RoomObject] -> [String]
        allNames = concat . extractNames
        extractNames :: [RoomObject] -> [[String]]
        extractNames = map (view objectAliases)

-- Check that a container contains something.
contains :: RoomObject  -- Container
            -> RoomObject -- Object name
            -> Bool
contains container contained = contained `elem` (_content . _rodetails $ container)

-- Pick an object contained inside a container.
pickItemFromContainer :: RoomObject         -- The container 
                        -> String           -- The object to pick
                        -> WorldFeedback
pickItemFromContainer container x = do
                                        w <- get
                                        case containeds x w of  -- Is the object really in the container ?
                                            []          -> singleAnswer $ "What on earth are you talking about ?"
                                            [object]    -> pickItemFromContainer' container object
                                            (xs)        -> error "Ambiguous case. Not implemented yet."
    where
        containeds x w =  identifyWithContained x w
        pickItemFromContainer' :: RoomObject -> RoomObject -> WorldFeedback
        pickItemFromContainer' container contained  -- Is the container opened ?
            -- TODO - Should be refactored : container CONTAINS contained, since we checked in the calling method.
            -- TODO - Should be refactored : the method says "pick", yet the item is not added to the inventory.
            | container `contains` contained && isOpened container = do
                            removeItemFromContainer container contained
                            singleAnswer $ "You picked up " ++ (mainName contained)
            | not (isOpened container) = singleAnswer $ mainName container ++ " is not opened !"

lookInsideContainer :: RoomObject -> String -> WorldFeedback
lookInsideContainer ro s
    | isOpened ro = singleAnswer $ (headName . _ronames $ ro) ++ " is closed, you can't look inside."
    | otherwise = return . Just $ (s:displayContainerContent ro)
    where
        displayContainerContent ro = [mainName x| x <- (ro^.containedObjects)]

putInsideContainer :: RoomObject    -- The container
                        -> String   -- The object to put
                        -> String   -- Text if it works
                        -> WorldFeedback
putInsideContainer container objectName workingText
    | not $ isOpened container = singleAnswer "Errr... it's closed. You should open it first."
    | otherwise = do
                    w <- get
                    case getFromInventory objectName w of
                        Nothing -> singleAnswer "You don't have this object !"
                        Just contained -> do
                            putItemInContainer contained container 
                            return (Just [workingText])

putItemInContainer :: RoomObject
                    -> RoomObject
                    -> WorldFeedback
putItemInContainer container contained = do
                        item <- removeFromInventory $ contained
                        let newContainer = container & containedObjects .~ (item:(container^.containedObjects))
                        objects <- use worldObjects
                        worldObjects .= rebuildList objects container newContainer
                        return Nothing

-- To remove an item from a container, we must :
-- 1°) Redefine the container as "not containing the contained"
-- 2°) Add the contained object to the list of world's owned RoomObject 
removeItemFromContainer ::  RoomObject          -- Container
                            -> RoomObject       -- Contained
                            -> WorldFeedback
removeItemFromContainer container contained = do
                            let newContainer = container & containedObjects .~ (removeObjectFromList (container^.containedObjects) (mainName contained))
                            objects <- use worldObjects
                            worldObjects .= rebuildList objects container newContainer
                            addToInventory contained
                            objects <- use worldObjects
                            worldObjects .= contained:objects
                            return Nothing

-- INVENTORY MANAGEMENT

inventoryContains :: RoomObject
                -> State World Bool
inventoryContains r = do
                        w <- get
                        return $ r { _inRoom = playerPockets } `elem` (w^.playerObjects)

hasInInventory :: String    -- Name of an object
                -> World    -- World 
                -> Bool
hasInInventory name world = length (findObjectWithName name (world^.playerObjects)) > 0
    where
        findObjectWithName :: String -> [RoomObject] -> [RoomObject]
        findObjectWithName s ros = filter (canBeNamed s) ros

removeFromInventory :: RoomObject
                        -> State World RoomObject 
removeFromInventory item = do
                        let newItem = item { _inRoom = "" } 
                        objects <- use worldObjects
                        worldObjects .= rebuildList objects item newItem
                        return newItem

addToInventory :: RoomObject    -- The object to change
                -> WorldFeedback
addToInventory = changeRoom playerPockets 

getFromInventory :: String
                -> World
                -> Maybe RoomObject
getFromInventory name world = find (canBeNamed name) (world^.playerObjects)

-- CHANGING STATE

-- Change the status of an object
changeStatus :: RoomObject          -- The room object to change
                -> ObjectStatus     -- The new status
                -> WorldFeedback
changeStatus ro st = do
                        wos <- use worldObjects
                        worldObjects .= rebuildList wos ro (ro & objectStatus.~ st)
                        return Nothing

-- Change the room an object is stored in.
changeRoom :: String
            -> RoomObject        -- The room object to change
            -> WorldFeedback
changeRoom name ro = do
                        wos <- use worldObjects
                        worldObjects .= rebuildList wos ro (ro { _inRoom = name })
                        return Nothing

-- CONVERSATIONS
handleConversation :: Aliases -> Topic -> WorldFeedback
handleConversation conv = error "NIY"


-- Used when State does not need to be changed.
-- Given a string, will return the World "as it is" and the string.
singleAnswer :: String -> WorldFeedback
singleAnswer = return . Just . ((:[]))

-- MOVE
basicMove :: String -- Destination
            -> WorldFeedback
basicMove r = do
        w <- get
        current <- use currentRoom
        previousRoom .= current             -- Memory of previous room kept to allow Flee action
        currentRoom .= roomByString w r 
        displayCurrentRoom

flee :: WorldFeedback
flee = do
        w <- get
        currentRoom .= (w^.previousRoom)
        displayCurrentRoom
