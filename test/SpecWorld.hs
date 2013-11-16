----------------------------------------------
-- This test suite is the most important one for Lambdacula Engine.
-- It will check the rules are respected for the given scenario :
--
-- a) A world with two rooms. There is a door between the two of them, but it is closed.
-- b) The first room contains a test cube. The test cube contains a key.
-- c) The key opens the door.
-- d) The second room contains a test man.
-- e) Talking to test man about tests make the game comes to an end.
--
-- To be efficient, we need to test actions before some previous conditions
-- are met. For instance, opening the door with the key before retrieving the
-- key.
--
-----------------------------------------------

-- These are hspec tests.
import Test.Hspec
import Test.QuickCheck
import Control.Monad.State
import Control.Lens hiding (Action)
import Lambdacula.Action
import Lambdacula.World
import Lambdacula.GameData
import Lambdacula.WorldBuilder
import Lambdacula.Flow
import Lambdacula.Display
import Lambdacula.Reactions
import qualified Data.Map as Map
import qualified Data.Foldable as Fd
import Data.List

import System.Console.Haskeline

-- Constants --
examineString = "A simple test cube. Look, how pretty !" 
sayhello = "hello world !"
zilchReaction = [Display "Err... no."]

openCubeReaction :: [Reaction]
openCubeReaction = [ChangeStatus "cube" Opened, Display "You open the cube !"]

-- Reaction tuples
reactions_tests = [("cube", Take, Just "key", [], [PickFromContainer "cube" "key"])
    ,("cube", Examine, Nothing, [], [Display examineString])
    ,("cube", Open, Nothing, [], openCubeReaction)
    ,("cube", Zilch, Nothing, [], zilchReaction)
    ,("dude", Talk, Nothing, [], [Conversation charatopics charaanswers undefined])]

-- Rooms
trRooms = [Room "The test room" "You are standing in a non-existant place in a virtual world. This is a very good place to hold existential thoughts. Or test the system. But this is more or less the same thing, innit ?" Nada
    ,Room "A second room" "You are in a second room. It doesn't exist, like the first one; so really, you moved but you didn't move. I know, I know, this sounds absurd. And to a point, it is." Nada] 

-- Characters
charatopics = [("hello", ["hi", "yo"]), ("endgame", [])]
charaanswers = [("hello", sayhello),("endgame", "Just say end to me")]

-- Objects
trObjects = [Exit (ObjectNames ["A test door", "door"]) "The test room" (RoomObjectDetails Closed "A hermetically locked door" []) (Just (DoorInfo (Just "key")))  "A second room" 
            , makeExit ["south"] "A second room" "A door" "The test room"
            , RoomObject (ObjectNames ["cube", "the test cube","test cube"]) "The test room" (RoomObjectDetails Closed "There is a nice test cube here." [keyObject])
            , RoomObject (ObjectNames ["dude"]) "The test room" (RoomObjectDetails Nada "A dude" [simpleObject ["thing"] "" "A thing to be tested"])
            , simpleObject ["thingamabob"] "The test room" "An object used in a test"]

keyObject = simpleObject ["key", "a key"] "NOWHERE" "Nothing worth looking at"

-- World
world = buildWorld trRooms trObjects reactions_tests

-- The series of action the player is expected to perform,
-- in the proper order, in our test world.
properActions = [(Interaction Open "cube")
                ,(Complex Take "cube" "key")
                ,(Complex Use "door" "key")
                ,(Interaction Move "door")]

----- Utilities -----

-- Apply an action, get the first String result
testProceed x = head . fst $ runState (proceed x) world

-- Check re
probeReactions :: (String, Action, Maybe String) -> [Reaction]
probeReactions trio = fst $ runState (findReactions trio) world

-- Test a given reaction
testReaction :: Reaction -> (World -> Bool) -> Bool
testReaction reaction f = f resultingWorld
    where resultingWorld = snd $ runState (onlyDo reaction) world

-- Test the result of a series of reactions
testReactions :: [Reaction] -> (World -> Bool) -> Bool
testReactions reactions f = f resultingWorld
    where resultingWorld = snd $ runState (mapM onlyDo reactions) world

-- Do a list of actions. Save the world. Reload the world.
-- Applies a checking function on the world to make sure the loaded world
-- kept tracks of state-changes.
saveLoadAndCheck :: [PlayerAction] -> (World -> Bool) -> IO Bool
saveLoadAndCheck actions test = do
                                    save "test.ldcl" $ snd $ runState (multiProceed actions) world
                                    w <- (load trRooms reactions_tests "test.ldcl")
                                    return $ test w

-- Apply actions, then apply a boolean function on the world
-- to make sure state has changed.
checkWorld :: [PlayerAction] -> (World -> Bool) -> Bool
checkWorld x t = t . snd $ runState (multiProceed x) world

-- Do a series of actions
multiProceed :: [PlayerAction] -> WorldAction
multiProceed [] = return []
multiProceed [action] = proceed action
multiProceed (action:actions) = do
            proceed action
            multiProceed actions

-- Get the string result from an action
testFeedback act = fst $ runState (proceed act) world

---- Various boolean functions ----

-- Make sure the player has no objects.
playerInventoryIsEmpty :: World -> Bool
playerInventoryIsEmpty w = length(w^.playerObjects) == 0

-- Make sure the player has a given object.
playerInventoryContains :: RoomObject -> World -> Bool
playerInventoryContains ro w = ro { _inRoom = playerPockets } `elem` (w^.playerObjects)

-- Check the status of an object, make sure it is what we except.
checkStatus :: String               -- Name of the object
                -> ObjectStatus     -- Status this object should have
                -> World            -- The world
                -> Bool             -- Does the status match ?
checkStatus s st w = case find ((==) s . mainName) (_worldObjects w) of
                    Just x -> x^.objectStatus == st 
                    Nothing -> error "Test poorly written !" 

-- Make sure the currentRoom is the one we believe it should be
checkCurrentRoom :: String -> World -> Bool
checkCurrentRoom s w = view currentRoomName w  == s

-- Make sure an item contains something
checkItemContains :: String     -- Container
                    -> String   -- Contained
                    -> World    -- Our test world
                    -> Bool
checkItemContains container contained w = contained `elem` map (mainName) (view containedObjects getObject)
    where 
        getObject = case identify container w of
                        [] -> error $ "No object named " ++ container ++ " !"
                        xs -> head xs

main :: IO()
main = hspec $ do
    describe "reaction matching" $ do
            it "Finds the proper reaction when trying to open the cube" $ do
                probeReactions ("cube", Open, Nothing) `shouldBe` openCubeReaction
            it "Finds no reaction when trying to move the dude" $ do
                probeReactions ("dude", Move, Nothing) `shouldBe` []
            it "Make sure the Zilch case is used when no other case work" $ do
                probeReactions ("cube", Eat, Nothing) `shouldBe` zilchReaction

    describe "reaction processing" $ do
            it "Check ChangeStatus" $ do
                testReaction (head openCubeReaction) (checkStatus "cube" Opened) `shouldBe` True

            it "Check PickFromContainer - when not opened, can't pick" $ do
                testReaction (PickFromContainer "cube" "key") (playerInventoryIsEmpty) `shouldBe` True

            it "Check PickFromContainer - when opened, pick" $ do
                testReactions ([ChangeStatus "cube" Opened, PickFromContainer "cube" "key"]) (playerInventoryIsEmpty) `shouldBe` False

            it "Check GetFromCharacter" $ do
                testReaction (GetFromCharacter "dude" "thing") (playerInventoryIsEmpty) `shouldBe` False

            it "Check PutInsideContainer - doesn't have object, container opened - should fail" $ do
                testReactions ([ChangeStatus "cube" Opened, PutInsideContainer "cube" "thingamabob" ""]) (checkItemContains "cube" "thingamabob") `shouldBe` False

            it "Check PutInsideContainer - has object, container closed - should fail" $ do
                testReactions ([PickItem "thingamabob", PutInsideContainer "cube" "thingamabob" ""]) (checkItemContains "cube" "thingamabob") `shouldBe` False

            it "Check PutInsideContainer - has object, container opened - should work" $ do
                testReactions ([PickItem "thingamabob", ChangeStatus "cube" Opened, PutInsideContainer "cube" "thingamabob" ""]) (checkItemContains "cube" "thingamabob") `shouldBe` True

    describe "String feedback" $ do
            it "Make sure the proper string is displayed when examining the cube" $ do
                testFeedback (Interaction Examine "cube") `shouldBe` [examineString]

    describe "conversation" $ do
            it "Make sure the proper string is displayed when talking to the dude" $ do
                testFeedback (Complex Talk "dude" "hello") `shouldBe` ["\"" ++ sayhello ++ "\""]

            it "Make sure the hello string is displayed when talking with no subject" $ do
                testFeedback (Interaction Talk "dude") `shouldBe` ["\"" ++ sayhello ++ "\""]

            it "Make sure the hello string is displayed when using an alias" $ do
                testFeedback (Complex Talk "dude" "yo") `shouldBe` ["\"" ++ sayhello ++ "\""]

    describe "scenario" $ do
            it "Tries to take the key out of the cube but fails, because it is closed" $ do
                checkWorld ([Complex Take "cube" "key"]) playerInventoryIsEmpty `shouldBe` True 

            it "Tries to go to the new room but fails because the door is closed" $ do
                checkWorld ([Interaction Move "north"]) (checkCurrentRoom "The test room") `shouldBe` True

            it "Opens the cube" $ do
                checkWorld ([Interaction Open "cube"]) (checkStatus "cube" Opened) `shouldBe` True

            it "Tries to open the door while not having the key" $ do
                checkWorld ([(properActions !! 0), (properActions !! 2)]) (checkStatus "A test door" Closed) `shouldBe` True

            it "Tries to pick up the key without opening the cube first and fails" $ do
                checkWorld ([properActions !! 1]) playerInventoryIsEmpty `shouldBe` True

            it "Open the cube, takes the key" $ do
                checkWorld (take 2 properActions) (playerInventoryContains keyObject) `shouldBe` True

            it "Open the cube, take the key and open the door" $ do
                checkWorld (take 3 properActions) (checkStatus "A test door" Opened) `shouldBe` True

            it "Do every action to open the door and cross it" $ do
                checkWorld (take 4 properActions) (checkCurrentRoom "A second room") `shouldBe` True

    describe "saving" $ do
            it "Checks that the world can be saved" $ do
                saveLoadAndCheck (take 4 properActions) (checkCurrentRoom "A second room") >>= (`shouldBe` True)
