-- These are hspec tests.
import Test.Hspec
import Test.QuickCheck
import Action
import Parser

speak = Transitive Talk "speak" ["with", "to"] ["about"]
ask = Transitive Talk "ask" ["about"] []
lookFor = Phrasal Search "look" "for" [] ["in", "with"]
quit = Transitive QuitGame "quit" [] []
verbs = [speak, ask, lookFor, quit]



main :: IO()
main = hspec $ do
    describe "processInput" $ do
            it "Finds the transitive verb Speak to in a sentence" $ do
                processInput "Speak to the neighbour" verbs `shouldBe` Interaction Talk "neighbour"

            it "Doesn't find a verb in a sentence" $ do
                processInput "Behold the power of the specs" verbs `shouldBe` SimpleAction Zilch

            it "Finds the phrasal verb \"Look for\" in a sentence" $ do
                processInput "Look for an answer to this riddle" verbs `shouldBe` Interaction Search "answer to this riddle"

            it "Finds a complex action with two objets" $ do
                processInput "Speak with the count about the meaning of life" verbs `shouldBe` Complex Talk "count" "meaning of life"
