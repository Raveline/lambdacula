module GameData where

import Control.Monad.State
import qualified Data.Map as Map
import Action
import World

-- VERBS
speak = Transitive Talk "speak" ["with", "to"] ["about"]
talk = Transitive Talk "talk" ["with", "to"] ["about"]
ask = Transitive Talk "ask" ["about"] []
lookFor = Phrasal Search "look" "for" [] ["in", "with"]
examine = Transitive Examine "examine" [] ["with"]
lookAt = Phrasal Examine "look" "at" [] ["with"]
look = Transitive Examine "look" [] ["with"] 
analyze = Transitive Examine "analyze" [] []
go = Transitive Move "go" [] []
eat = Transitive Eat "eat" [] []
quit = Transitive QuitGame "quit" [] []

-- OBJECTS
testCube = RoomObject "the test cube" ["test cube", "cube"] useTestCube

useTestCube :: Action -> State World [String] 
useTestCube Examine = singleAnswer "A simple test cube. Look, how pretty !"
useTestCube Talk = singleAnswer "You can't talk to a cube, don't be silly."
useTestCube Move = singleAnswer "You push the cube. Happy now ?"
useTestCube Open = singleAnswer "You don't find any opening on the cube"
useTestCube Eat = singleAnswer "You try to eat the cube. It's not very good. Particularly for your teeth."
useTestCube _ = singleAnswer "You can't do that to the test cube" -- TO CHANGE. Return empty string, and deal with this input in proceed. 


verbs = [speak, talk, ask, lookFor, lookAt, examine, look, analyze, go, eat, quit]

fromOneToTwo = Exit "north" [] "a weird discontinuity in space and time" (basicMove room') True 
fromTwoToOne = Exit "south" [] "a passage that defies the law of physics" (basicMove room) True 

room = Room "The test room" "You are standing in a non-existant place in a virtual world. This is a very good place to hold existential thoughts. Or test the system. But this is more or less the same thing, innit ?\nThere is a nice **test cube** in the center of the non-space." [testCube] [] [fromOneToTwo] 
room' = Room "A second room" "You are in a second room. It doesn't exist, like the first one; so really, you moved but you didn't move. I know, I know, this sounds absurd. And to a point, it is." [] [] [fromTwoToOne]
