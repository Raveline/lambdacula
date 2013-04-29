module Lambdacula.GameData where

import Control.Monad.State
import qualified Data.Map as Map
import Lambdacula.Action
import Control.Lens hiding (Action)
import Lambdacula.World
import Lambdacula.Display

type WorldSituation = State World ()
type MoveAction = Room -> RoomObject -> Action -> State World [String]

-- VERBS
speak = Transitive Talk "speak" ["with", "to"] ["about"]
talk = Transitive Talk "talk" ["with", "to"] ["about"]
ask = Transitive Talk "ask" ["about"] []
lookFor = Phrasal Search "look" "for" [] ["in", "with"]
examine = Transitive Examine "examine" [] ["with"]
lookAt = Phrasal Examine "look" "at" [] ["with"]
look = Transitive Examine "look" [] ["with"] 
open = Transitive Open "open" [] []
analyze = Transitive Examine "analyze" [] []
go = Transitive Move "go" [] []
eat = Transitive Eat "eat" [] []
quit = Transitive QuitGame "quit" [] []

verbs = [speak, talk, ask, lookFor, lookAt, examine, look, analyze, go, eat, quit, open]
-- OBJECTS
simpleObject :: [String] -> RoomObjectBehaviour -> String -> RoomObject 
simpleObject aliases reaction description = RoomObject naming reaction details
    where
        naming = ObjectNames aliases
        details = RoomObjectDetails Nada description [] 

testCube = RoomObject (ObjectNames ["the test cube","test cube", "cube"]) (useTestCube) (RoomObjectDetails Closed "A simple test cube. Look, how pretty !" [basicObject])
basicObject = simpleObject ["a thingy", "thingy"] noReaction "Nothing worth looking at"

useTestCube :: RoomObject -> Action -> WorldAction 
useTestCube cube Examine = singleAnswer (cube^.objectDescription)
useTestCube _ Talk = singleAnswer "You can't talk to a cube, don't be silly."
useTestCube _ Move = singleAnswer "You push the cube. Happy now ?"
useTestCube cube Open = openContainer cube "You open the cube !"
useTestCube _ Eat = singleAnswer "You try to eat the cube. It's not very good. Particularly for your teeth."
useTestCube _ _ = singleAnswer "You can't do that to the test cube" -- TO CHANGE. Return empty string, and deal with this input in proceed. 

-- Given a room object, and a success string, open the container if possible
-- and display its content
openContainer :: RoomObject -> String -> WorldAction
openContainer ro sust
    | isOpened ro = singleAnswer "It's already opened !"
    | otherwise = do
                    w <- get
                    
                    let
                        ro' = ro & objectStatus .~ Opened
                        newWorld = updateCurrentRoom w (updateRoomObjects (view currentRoom w) ro ro')
                    put newWorld
                    return (sust:"It contains : ":displayContainerContent ro)


noReaction :: RoomObject -> Action -> WorldAction
noReaction _ _ = singleAnswer "This object is just for tests."

basicMove :: MoveAction
basicMove r _ Move = do
                    w <- get
                    put $ w { _currentRoom = r } 
                    return $ displayRoom r
basicMove _ _ _ = singleAnswer "What on earth are you trying to do ?"

makeExit :: String -> [String] -> String -> MoveAction -> Room -> ObjectStatus -> RoomObject
makeExit name aliases description action room status = Exit (ObjectNames (name:aliases)) (basicMove room) (RoomObjectDetails Nada description []) 

fromOneToTwo = makeExit "north" [] "a weird discontinuity in space and time" basicMove room' Opened 
fromTwoToOne = makeExit "south" [] "a passage that defies the law of physics" basicMove room Opened

room = Room "The test room" "You are standing in a non-existant place in a virtual world. This is a very good place to hold existential thoughts. Or test the system. But this is more or less the same thing, innit ?\nThere is a nice **test cube** in the center of the non-space." [testCube, fromOneToTwo] 
room' = Room "A second room" "You are in a second room. It doesn't exist, like the first one; so really, you moved but you didn't move. I know, I know, this sounds absurd. And to a point, it is." [fromTwoToOne]

arrival = Room "In front of the castle" "You're standing in front of the castle of Lambdacula, in the heart of transylvania. It is standing at the top of a moutain, as any proper gothic castle should be. In front of you, to the south, the gates of the castle lead to the inner yard. I could describe the howling wind, the eerie atmosphere, the uncanny mist, the noise of flapping bats and other items from my Dictionnary Of Transylvanian Clichés, but I think you've got the idea. To the south, you'll find the gate of the castle, that you can cross to enter into an inner yard. On the east, a little path should lead you to safety or towards new adventures, but, come on, try to finish this one first." [makeExit "south" [] "the gate of the castle" basicMove southernYard Opened]

southernYard = Room "The southern gate" "This is the inner yard of the castle. Obviously, count Lambdacula must have financial trouble, or his skills in household management are more than lacking. The place is a wreck, let's face it. There is an awful stench everywhere, rats are running through the place, the windows are dusty and frankly, UNESCO World Heritage Centre would be appalled by this place. There is a pile of junk next to the gates of the castle, and if you stay there too long, I'd say a pool of vomit close to it." [makeExit "north" [] "the gate of the castle" basicMove arrival Opened] 

easternPath = Room "A muddy path" "You're walking a long a path that goes down the mountain of Lambda, where the castle of Lambdacula was built. You'd like to go along the path more and leave this dreadful place to go back to civilization but a strange feeling prevent you from... oh, who am I kidding ? You can't escape, you coward, you've got to face the count and murder him, because this is the goal of this sick, twisted, vampirophobic videogame. If you've got an issue with that, just type \"quit\" and be gone.\nWhere was I ? So, yes, the path could let you go away. The only interesting place here is a shack. It's most likely where the gamekeeper of Count Lambdacula used to live. Well, there is the slight possibility that the gamekeeper is immortal and still lives there, so you never know, that's the issue with undeads." [makeExit "west" [] "an upward path to the castle" basicMove arrival Opened]

-- The shack
gameKeeperShack = Room "The gamekeeper shack" "You're inside a simple shack made out of rudimentary logs. Everything is covered with spider webs. Apparently, no one has been there for a long time, though you could swear there are some footsteps on the dusty floorboards. On the wall opposite to the door, there is something that must have been a bed long ago. Next to it, a night table with an old copy of \"Lady's Chatterley's Lover\". There is also a cupboard, a diner table that was half-eaten by termites and a big bucket that must have been used for some very basic form of washing up. Decorum is not really the place forte, though there is a nice rug on the floor." [makeExit "door" [] "a way out to the path below Lambdacula's Castle" basicMove easternPath Opened]


buildWorld :: [Room] -> WorldSituation
buildWorld [] = state $ \w -> ((), w)
buildWorld (r:rs) = do
                    wr <- use worldRooms
                    worldRooms .= (r:wr)
                    buildWorld rs
