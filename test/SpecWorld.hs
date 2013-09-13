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
import Data.List

import System.Console.Haskeline

examineString = "A simple test cube. Look, how pretty !" 
zilchString = "You can't do that to the test cube" 
noSuchObjectString = "I did not understand what you want to do with gizmo, sorry."
cubeWithWeather = "The cube has nothing to say about the weather."

openCubeReaction :: [Reaction]
openCubeReaction = [ChangeStatus "cube" Opened, Display "You open the cube !"]

-- Reaction tuples
reactions_tests = [("cube", Talk, Just "Weather", [], [Display cubeWithWeather])
    ,("cube", Take, Just "key", [], [PickFromContainer "cube" "key"])
    ,("cube", Examine, Nothing, [], [Display examineString])
    ,("cube", Talk, Nothing, [], [Display "You can't talk to a cube, don't be silly"])
    ,("cube", Open, Nothing, [], openCubeReaction)
    ,("cube", Eat, Nothing, [], [Display "You try to eat the cube. It's not very good. Particularly for your teeth."])]

trRooms = [Room "The test room" "You are standing in a non-existant place in a virtual world. This is a very good place to hold existential thoughts. Or test the system. But this is more or less the same thing, innit ?" Nada
            , Room "A second room" "You are in a second room. It doesn't exist, like the first one; so really, you moved but you didn't move. I know, I know, this sounds absurd. And to a point, it is." Nada] 

trObjects = [Exit (ObjectNames ["A test door", "door"]) "The test room" (RoomObjectDetails Closed "A hermetically locked door" []) (Just (DoorInfo (Just "key")))  "A second room" 
            , makeExit ["south"] "A second room" "A door" "The test room"
            , RoomObject (ObjectNames ["cube", "the test cube","test cube"]) "The test room" (RoomObjectDetails Closed "There is a nice test cube here." [keyObject])]

keyObject = simpleObject ["key", "a key"] "NOWHERE" "Nothing worth looking at"

world = buildWorld trRooms trObjects reactions_tests
testProceed x = head . fst $ runState (proceed x) world

-- Get the reaction after processing them
probeReactions :: (String, Action, Maybe String) -> [Reaction]
probeReactions trio = fst $ runState (findReactions trio) world

-- Test a given reaction
testReaction :: Reaction -> (World -> Bool) -> Bool
testReaction reaction f = f resultingWorld
    where resultingWorld = snd $ runState (onlyDo reaction) world

checkWorld :: [PlayerAction] -> (World -> Bool) -> Bool
checkWorld x t = t . snd $ runState (multiProceed x) world
    where 
        multiProceed :: [PlayerAction] -> WorldAction
        multiProceed [] = return []
        multiProceed [action] = proceed action
        multiProceed (action:actions) = do
                                            proceed action
                                            multiProceed actions

playerInventoryIsEmpty :: World -> Bool
playerInventoryIsEmpty w = length(w^.playerObjects) == 0

playerInventoryContains :: RoomObject -> World -> Bool
playerInventoryContains ro w = ro { _inRoom = playerPockets } `elem` (w^.playerObjects)

checkStatus :: String               -- Name of the object
                -> ObjectStatus     -- Status this object should have
                -> World            -- The world
                -> Bool             -- Does the status match ?
checkStatus s st w = case find ((==) s . mainName) (_worldObjects w) of
                    Just x -> x^.objectStatus == st 
                    Nothing -> error "Test poorly written !" 

checkCurrentRoom :: String -> World -> Bool
checkCurrentRoom s w = (_roomName . _currentRoom $ w) == s

properActions = [(Interaction Open "cube")
                ,(Complex Take "cube" "key")
                ,(Complex Use "door" "key")
                ,(Interaction Move "door")]

-- Just a lambdacula-repl loader to test things with GHCI
repl = do
    printStrs $ displayRoom (view currentRoom world) (view currentObjects world)
    runInputT defaultSettings (promptLoop world)

main :: IO()
main = hspec $ do
    describe "reaction matching" $ do
            it "Finds the proper reaction when trying to open the cube" $ do
                probeReactions ("cube", Open, Nothing) `shouldBe` openCubeReaction
            it "Finds no reaction when trying to move the cube" $ do
                probeReactions ("cube", Move, Nothing) `shouldBe` []

    describe "reaction processing" $ do
            it "Process the open cube reaction and make sure it works !" $ do
                testReaction (head openCubeReaction) (checkStatus "cube" Opened) `shouldBe` True

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
