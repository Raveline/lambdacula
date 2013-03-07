-- These are hspec tests.
import Test.Hspec
import Test.QuickCheck
import Action
import World
import qualified Data.Map as Map

testString = "You don't find any opening on the cube."
testFailure = "You can't do that to the test cube !"
testLook = "A simple test cube. Look, how pretty !"
testCube = RoomObject "the test cube" ["Test cube", "cube"] (Map.fromList[(Examine, testLook),
                                            (Talk, "You can't talk to a cube, don't be silly"),
                                            (Move, "You push the cube. Happy now ?"),
                                            (Open, testString)]) 

main :: IO()
main = hspec $ do
    describe "processInput" $ do
            it "Finds the action Move on theverb Speak to in a sentence" $ do
                getTextForAction testCube Open `shouldBe` testString

            it "Finds the action Examine on the verb Examine in a sentence." $ do
                getTextForAction testCube Examine `shouldBe` testLook

            it "Finds no action and print a default error" $ do
                getTextForAction testCube Zilch `shouldBe` testFailure
