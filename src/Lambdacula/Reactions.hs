-- Please note. Reactions use only String alias.
-- This string will be "converted" to the proper, "real" roomObject.
-- Which means you should only use UNIQUE string for reactions.
-- Runtime error will happen if you don't.

module Lambdacula.Reactions
( 
    Reaction (..),
    Condition (..),
    Reactions,
    ReactionSet,
    processAction,
    onlyDo,
    onlyDisplay
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

type Reactions = [Reaction]
type ReactionSet = (String, Action, Maybe String, Reactions)

type PureActionDetail = (String, Action, Maybe String)
type Feedback = Maybe [String]
type WorldFeedback = State World Feedback

type Aliases = [(String, [String])]
type Topic = [(String, String)]

data Reaction =  Display String                             -- Display some text
                | PickItem String                           -- Add to inventory
                | ChangeStatus String ObjectStatus          -- Change object status
                | PickFromContainer String String           -- Pick from container (1) object (2)
                | LookInsideContainer String String         -- Look content of container (1) with intro sentence (2)
                | PutInsideContainer String String String   -- Put inside container (1) the item (2) with resulting sentence (3)
                | RebranchTo Action String (Maybe String)   -- Rephrase a command so that it'll be retranslated 
                | Conversation [(String, [String])] [(String, String)] 
                | MoveTo String                             -- Move somewhere
                | Flight                                    -- Flee
                | LookAround                                -- Display current room
                | Error                                     -- Not Implemented Yet 

onlyDisplay :: String -> WorldAction
onlyDisplay s = onlyDo $ Display s

onlyDo :: Reaction -> WorldAction
onlyDo r = do
                feedback <- processReaction r
                case feedback of 
                    Just s' -> return s'
                    Nothing -> return []
                    

processAction :: (RoomObject, Action, Maybe RoomObject) -> WorldAction
processAction ((Exit _ _ _ info dest), a, x) = handleExit info dest a x
processAction (x, a, Just (Exit _ _ _ info dest)) = handleExit info dest a (Just x)
processAction detail = do
                    reactions <- findReactions $ simplifyAction detail
                    Fd.foldlM process [] reactions
    where
        process :: [String] -> Reaction -> State World [String]
        process strs reac = do
                    feedback <- processReaction reac 
                    return $ extractStr strs feedback
        extractStr :: [String] -> Feedback -> [String]
        extractStr strs (Just s) = s ++ strs
        extractStr strs Nothing = strs 

handleExit :: Maybe DoorInfo -> String -> Action -> Maybe RoomObject -> WorldAction
handleExit = error "NIY, and probably should not be implemented."

findReactions :: PureActionDetail -> State World [Reaction]
findReactions = error "NIY"

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
processReaction (MoveTo location) = error "NIY"

data Condition = ContainsAmountOfItem Int
                | PlayerHasStatus ObjectStatus
                | HasStatus ObjectStatus
                | Contains String

testCondition :: Condition -> World -> RoomObject -> Bool
testCondition (ContainsAmountOfItem x) w r = (==) x . length . view containedObjects $ r
testCondition (PlayerHasStatus stat) w r = error "NIY"
testCondition (HasStatus stat) w r = (==) stat . view objectStatus $ r
testCondition (Contains name) w ro = ro `containsSomethingNamed` name  

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
                case find (\ro -> (mainName ro) == s) scope of
                    Nothing -> error "Object not found in scope. This should not happen."
                    Just x -> return x

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
                                        case containeds x w of
                                            []          -> singleAnswer $ "What on earth are you talking about ?"
                                            [object]    -> pickItemFromContainer' container object
                                            (xs)        -> error "Ambiguous case. Not implemented yet."
    where
        containeds x w =  identifyWithContained x w
        pickItemFromContainer' :: RoomObject -> RoomObject -> WorldFeedback
        pickItemFromContainer' container contained
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
basicMove :: String -> RoomObject -> Action -> WorldFeedback
basicMove r passage Move 
    | passage^.objectStatus == Opened = do
                    w <- get
                    current <- use currentRoom
                    previousRoom .= current 
                    currentRoom .= roomByString w r 
                    displayCurrentRoom
    | otherwise = singleAnswer "You can't, the path is closed !"
basicMove _ _ _ = singleAnswer "What on earth are you trying to do ?"

flee :: WorldFeedback
flee = do
        w <- get
        currentRoom .= (w^.previousRoom)
        displayCurrentRoom
