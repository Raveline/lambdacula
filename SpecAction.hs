-- These are hspec tests.
import Test.Hspec
import Test.QuickCheck
import Action

verbExample = Transitive "speak" ["with", "to"]
verbExample' = Transitive "ask" ["about"]
verbExample'' = Phrasal "look" "for" []
verbs = [verbExample, verbExample', verbExample'']


main :: IO()
main = hspec $ do
    describe "processInput" $ do
            it "Finds the transitive verb Speak to in a sentence" $ do
                processInput "Speak to the neighbour" verbs `shouldBe` Just(verbExample)

            it "Doesn't find a verb in a sentence" $ do
                processInput "Behold the power of the specs" verbs `shouldBe` Nothing

            it "Finds the phrasal verb \"Look for\" in a sentence" $ do
                processInput "Look for an answer to this riddle" verbs `shouldBe` Just(verbExample'')
