-- These are hspec tests.
import Test.Hspec
import Test.QuickCheck
import Control.Monad.State
import Action
import World
import Lambdacula 
import GameData
import qualified Data.Map as Map

testString = "You don't find any opening on the cube"
testFailure = "You can't do that to the test cube"
testLook = "A simple test cube. Look, how pretty !"
testNoSuchObject = "There is no gizmo here !"

world = World (Player []) room (mapFromRooms [room, room'])
testProceed x = head . fst $ runState (proceed x) world

main :: IO()
main = hspec $ do
    describe "processInput" $ do
            it "Finds the action Move on theverb Speak to in a sentence" $ do
                testProceed (Interaction Open "cube") `shouldBe` testString

            it "Finds the action Examine on the verb Examine in a sentence." $ do
                testProceed (Interaction Examine "cube") `shouldBe` testLook

            it "Finds no action and print a default error" $ do
                testProceed (Interaction Zilch "cube") `shouldBe` testFailure

            it "Doesn't find an object and says so" $ do
                testProceed (Interaction Open "gizmo") `shouldBe` testNoSuchObject
