-- These are hspec tests.
import Test.Hspec
import Test.QuickCheck
import Action
import World
import qualified Data.Map as Map

testString = "You don't find any opening on the cube."
testFailure = "You can't do that to the test cube !"
testCube = RoomObject "the test cube" ["Test cube", "cube"] (Map.fromList[(Examine, "A simple test cube. Look, how pretty !"),
                                            (Talk, "You can't talk to a cube, don't be silly"),
                                            (Move, "You push the cube. Happy now ?"),
                                            (Open, testString)]) 

main :: IO()
main = hspec $ do
    describe "processInput" $ do
            it "Finds the action Move on theverb Speak to in a sentence" $ do
                getTextForAction Open testCube `shouldBe` testString

            it "Finds no action and print a default error" $ do
                getTextForAction Zilch testCube `shouldBe` testFailure
