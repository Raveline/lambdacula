-- These are hspec tests.
import Test.Hspec
import Test.QuickCheck
import Action
import Parser

speak = Transitive Talk "speak" ["with", "to"] ["about"]
talk = Transitive Talk "talk" ["with", "to"] ["about"]
ask = Transitive Talk "ask" ["about"] []
lookFor = Phrasal Search "look" "for" [] ["in", "with"]
examine = Transitive Examine "examine" [] ["with"]
look = Transitive Examine "look" [] ["with"] 
lookAt = Phrasal Examine "look" "at" [] ["with"]
analyze = Transitive Examine "analyze" [] []
quit = Transitive QuitGame "quit" [] []
verbs = [speak, talk, ask, lookFor, lookAt, examine, look, analyze, quit]

main :: IO()
main = hspec $ do
    describe "processInput" $ do
            it "Finds the transitive verb Speak to in a sentence" $ do
                processInput "Speak to the neighbour" verbs `shouldBe` Interaction Talk "neighbour"

            it "Doesn't find a verb in a sentence" $ do
                processInput "Behold the power of the specs" verbs `shouldBe` SimpleAction Zilch

            it "Finds the phrasal verb \"Look for\" in a sentence" $ do
                processInput "Look for an answer to this riddle" verbs `shouldBe` Interaction Search "answer to this riddle"

            it "Decode \"Examine the cube\" as an Interaction Examine cube" $ do
                processInput "Examine the cube" verbs `shouldBe` Interaction Examine "cube"

            it "Finds a complex action with two objets" $ do
                processInput "Speak with the count about the meaning of life" verbs `shouldBe` Complex Talk "count" "meaning of life"

            it "Finds the SimpleAction Examine" $ do
                processInput "Look" verbs `shouldBe` SimpleAction Examine

            it "Shows that Look can be a phrasal (look at) as well as a transitive verb." $ do
                processInput "Look at the painting" verbs `shouldBe` Interaction Examine "painting"
