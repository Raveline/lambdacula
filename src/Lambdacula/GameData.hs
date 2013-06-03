module Lambdacula.GameData where

import Control.Monad.State
import qualified Data.Map as Map
import Lambdacula.Action
import Control.Lens hiding (Action)
import Lambdacula.World
import Lambdacula.Display
import Lambdacula.ModelShortcuts

type MoveAction = String -> RoomObject -> Action -> Maybe String -> State World [String]

-- VERBS
verbs = [Transitive Talk "speak" ["with", "to"] ["about"]
        ,Transitive Talk "talk" ["with", "to"] ["about"]
        ,Transitive Talk "ask" ["about"] []
        ,Phrasal Search "look" "for" [] ["in", "with"] True
        ,Transitive Examine "examine" [] ["with"]
        ,Phrasal Examine "look" "at" [] ["with"] False
        ,Transitive Examine "look" [] ["with"] 
        ,Transitive Open "open" [] []
        ,Transitive Examine "analyze" [] []
        ,Transitive Move "go" [] []
        ,Transitive Eat "eat" [] []
        ,Transitive QuitGame "quit" [] []
        ,Transitive Take "take" [] []
        ,Transitive Use "use" [] ["on"]
        ,Transitive Use "open" [] ["with"]
        ,Transitive Use "unlock" [] ["with", "using"]
        ,Phrasal Take "pick" "up" [] ["from", "in", "on"] True
        ,Transitive Take "take" [] ["from", "of", "out"] True 
        ,Transitive Inventorize "inventory" [] []]

-- OBJECTS
simpleObject :: [String]                -- Names 
                -> String               -- Room
                -> RoomObjectBehaviour  -- Way it reacts
                -> String               -- Description
                -> RoomObject           -- A proper room object
simpleObject aliases room reaction description = RoomObject naming room reaction details
    where
        naming = ObjectNames aliases
        details = RoomObjectDetails Nada description [] 

basicMove :: MoveAction
basicMove r passage Move _ 
    | passage^.objectStatus == Opened = do
                    w <- get
                    currentRoom .= roomByString w r 
                    displayCurrentRoom 
    | otherwise = singleAnswer "You can't, the path is closed !"
basicMove _ _ _ _ = singleAnswer "What on earth are you trying to do ?"

makeExit :: String          -- Main name
            -> [String]     -- Aliases
            -> String       -- Room where this exit should be
            -> String       -- Description of the object
            -> String       -- Destination
            -> ObjectStatus -- Opened / Closed ?
            -> RoomObject   -- An Exit
makeExit name aliases inRoom description room status = Exit (ObjectNames (name:aliases)) inRoom (basicMove room) (RoomObjectDetails Nada description []) room 


ldRooms = [Room "In front of the castle" "You're standing in front of the castle of Lambdacula, in the heart of transylvania. It is standing at the top of a moutain, as any proper gothic castle should be. In front of you, to the south, the gates of the castle lead to the inner yard. I could describe the howling wind, the eerie atmosphere, the uncanny mist, the noise of flapping bats and other items from my Dictionnary Of Transylvanian Clichés, but I think you've got the idea. To the south, you'll find the gate of the castle, that you can cross to enter into an inner yard. On the east, a little path should lead you to safety or towards new adventures, but, come on, try to finish this one first." 
        , Room "The southern gate" "This is the inner yard of the castle. Obviously, count Lambdacula must have financial trouble, or his skills in household management are more than lacking. The place is a wreck, let's face it. There is an awful stench everywhere, rats are running through the place, the windows are dusty and frankly, UNESCO World Heritage Centre would be appalled by this place. There is a pile of junk next to the gates of the castle, and if you stay there too long, I'd say a pool of vomit close to it." 
        , Room "A muddy path" "You're walking a long a path that goes down the mountain of Lambda, where the castle of Lambdacula was built. You'd like to go along the path more and leave this dreadful place to go back to civilization but a strange feeling prevent you from... oh, who am I kidding ? You can't escape, you coward, you've got to face the count and murder him, because this is the goal of this sick, twisted, vampirophobic videogame. If you've got an issue with that, just type \"quit\" and be gone.\nWhere was I ? So, yes, the path could let you go away. The only interesting place here is a shack. It's most likely where the gamekeeper of Count Lambdacula used to live. Well, there is the slight possibility that the gamekeeper is immortal and still lives there, so you never know, that's the issue with undeads." 
        , Room "The gamekeeper shack" "You're inside a simple shack made out of rudimentary logs. Everything is covered with spider webs. Apparently, no one has been there for a long time, though you could swear there are some footsteps on the dusty floorboards. On the wall opposite to the door, there is something that must have been a bed long ago. Next to it, a night table with an old copy of \"Lady's Chatterley's Lover\". There is also a cupboard, a diner table that was half-eaten by termites and a big bucket that must have been used for some very basic form of washing up. Decorum is not really the place forte, though there is a nice rug on the floor." ]

ldObjects = [makeExit "south" [] "In front of the castle" "the gate of the castle" "The southern gate" Opened
            ,makeExit "north" [] "The southern gate" "the gate of the castle" "In front of the castle" Opened
            ,makeExit "west" [] "A muddy path" "an upward path to the castle" "In front of the castle" Opened
            ,makeExit "door" [] "The gamekeeper shack" "a way out to the path below Lambdacula's Castle" "A muddy path" Opened]
