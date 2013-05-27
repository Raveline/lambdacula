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
import Lambdacula.Action
import Lambdacula.World
import Lambdacula.GameData
import Lambdacula.WorldBuilder
import Lambdacula.Flow
import qualified Data.Map as Map


examineString = "A simple test cube. Look, how pretty !" 
zilchString = "You can't do that to the test cube" 
noSuchObjectString = "There is no gizmo here !"
cubeWithWeather = "The cube has nothing to say about the weather."

-- Test method for a test cube
useTestCube :: RoomObject -> Action -> Maybe String -> WorldAction 
useTestCube cube Talk (Just "weather") = singleAnswer cubeWithWeather
useTestCube cube Examine _ = singleAnswer examineString
useTestCube _ Talk _ = singleAnswer "You can't talk to a cube, don't be silly."
useTestCube _ Move _ = singleAnswer "You push the cube. Happy now ?"
useTestCube cube Open _ = openContainer cube "You open the cube !"
useTestCube _ Eat _ = singleAnswer "You try to eat the cube. It's not very good. Particularly for your teeth."
useTestCube cube Take _ = pickItem cube
useTestCube _ _ _ = singleAnswer zilchString 

trRooms = [Room "The test room" "You are standing in a non-existant place in a virtual world. This is a very good place to hold existential thoughts. Or test the system. But this is more or less the same thing, innit ?" 
            , Room "A second room" "You are in a second room. It doesn't exist, like the first one; so really, you moved but you didn't move. I know, I know, this sounds absurd. And to a point, it is."] 

trObjects = [makeExit "north" [] "The test room" "a weird discontinuity in space and time" "A second room" Opened
            , makeExit "south" [] "A second room" "a passage that defies the law of physics" "The test room" Opened
            , RoomObject (ObjectNames ["the test cube","test cube", "cube"]) "The test room" (useTestCube) (RoomObjectDetails Closed "There is a nice test cube here." [basicObject])]

noReaction :: RoomObject -> Action -> Maybe String -> WorldAction
noReaction _ _ _ = singleAnswer "This object is just for tests."

basicObject = simpleObject ["a thingy", "thingy"] "NOWHERE" noReaction "Nothing worth looking at"

world = buildWorld trRooms trObjects
testProceed x = head . fst $ runState (proceed x) world

main :: IO()
main = hspec $ do
    describe "processInput" $ do
            it "Finds the action Examine on the verb Examine in a sentence." $ do
                testProceed (Interaction Examine "cube") `shouldBe` examineString  

            it "Finds no action and print a default error" $ do
                testProceed (Interaction Zilch "cube") `shouldBe` zilchString

            it "Doesn't find an object and says so" $ do
                testProceed (Interaction Open "gizmo") `shouldBe` noSuchObjectString 

            it "Handle an interaction" $ do
                testProceed (Complex Talk "cube" "weather") `shouldBe` cubeWithWeather
