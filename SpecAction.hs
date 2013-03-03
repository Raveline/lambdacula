-- These are hspec tests.
import Test.Hspec
import Test.QuickCheck
import Action
import Parser

verbExample = Transitive Talk "speak" ["with", "to"] ["about"]
verbExample' = Transitive Talk "ask" ["about"] []
verbExample'' = Phrasal Search "look" "for" [] ["in", "with"]
verbs = [verbExample, verbExample', verbExample'']


main :: IO()
main = hspec $ do
    describe "processInput" $ do
            it "Finds the transitive verb Speak to in a sentence" $ do
                processInput "Speak to the neighbour" verbs `shouldBe` Interaction Talk (Object "the neighbour")

            it "Doesn't find a verb in a sentence" $ do
                processInput "Behold the power of the specs" verbs `shouldBe` SimpleAction Zilch

            it "Finds the phrasal verb \"Look for\" in a sentence" $ do
                processInput "Look for an answer to this riddle" verbs `shouldBe` Interaction Search (Object "an answer to this riddle")
