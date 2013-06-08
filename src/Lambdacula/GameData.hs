module Lambdacula.GameData where

import Control.Monad.State
import qualified Data.Map as Map
import Lambdacula.Action
import Control.Lens hiding (Action)
import Lambdacula.World
import Lambdacula.Display
import Lambdacula.ModelShortcuts


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
        ,Transitive Take "take" [] ["from", "of", "out"]
        ,Transitive Lift "lift" [] []
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


makeExit :: String          -- Main name
            -> [String]     -- Aliases
            -> String       -- Room where this exit should be
            -> String       -- Description of the object
            -> String       -- Destination
            -> RoomObject   -- An Exit
makeExit name aliases inRoom description room = Exit (ObjectNames (name:aliases)) inRoom (basicMove room) (RoomObjectDetails Opened description []) room 

makeDoor :: [String]     -- Aliases
            -> String       -- Room where this exit should be
            -> String       -- Description of the object
            -> String       -- Destination
            -> Maybe String -- Key ?
            -> ObjectStatus -- Opened / Closed / Hidden ?
            -> RoomObject   -- An Exit
makeDoor alias inRoom description room key status = Exit (ObjectNames alias) inRoom (moveDoor key room) (RoomObjectDetails status description []) room

ldRooms = [Room "In front of the castle" "You're standing in front of the castle of Lambdacula, in the heart of transylvania. It is standing at the top of a moutain, as any proper gothic castle should be. In front of you, to the south, the gates of the castle lead to the inner yard. I could describe the howling wind, the eerie atmosphere, the uncanny mist, the noise of flapping bats and other items from my Dictionnary Of Transylvanian Clichés, but I think you've got the idea. To the south, you'll find the gate of the castle, that you can cross to enter into an inner yard. On the east, a little path should lead you to safety or towards new adventures, but, come on, try to finish this one first." Nada
        , Room "The southern gate" "This is the inner yard of the castle. Obviously, count Lambdacula must have financial trouble, or his skills in household management are more than lacking. The place is a wreck, let's face it. There is an awful stench everywhere, rats are running through the place, the windows are dusty and frankly, UNESCO World Heritage Centre would be appalled by this place. There is a pile of junk next to the gates of the castle, and if you stay there too long, I'd say a pool of vomit close to it." Nada
        , Room "The kitchen" "You're standing in a vast room, specifically designed to the production of fine food. Well, it used to be, because like everything else here, it has seen better days. Everything food related has been eaten by rats, insects and even less pleasant animals. Dusty or broken tableware lay on the tables. Empty, opened cupboard made out of rotting wood are waiting for a mercyful soul to chop their soul out of here. Needless to say, there is no fridge, no coffee machine, not the slightest touch of modern comfort. Making anything edible in here would be quite a feat." Nada
        , Room "The guard rooms" "In the middle age, this room would have been packed with tough looking men-at-arms, patrolling merrily, freezing their asses around the braseros, singing politically uncorrect songs. Nowadays, it's just a big room, almost empty, though the braseros are still there. It's a big room with six heavy columns. Though the walls are sometimes cracked, you think it's not going to crumble before long, so feel free to frolick around as much as you need." Nada
        , Room "A muddy path" "You're walking along a path that goes down the mountain of Lambda, where the castle of Lambdacula was built. You'd like follow the path onwards and leave this dreadful place to go back to civilization but a strange feeling prevent you from... oh, who am I kidding ? You can't escape, you coward, you've got to face the count and murder him, because this is the goal of this sick, twisted, vampirophobic videogame. If you've got an issue with that, just type \"quit\" and be gone.\nWhere was I ? So, yes, the path could let you get away. The only interesting place here is a shack. It's most likely where the gamekeeper of Count Lambdacula used to live. Well, there is the slight possibility that the gamekeeper is immortal and still lives there, so you never know, that's the issue with undeads." Nada
        , Room "The gamekeeper shack" "You're inside a simple shack made out of rudimentary logs. Everything is covered with spider webs. Apparently, no one has been there for a long time, though you could swear there are some footsteps on the dusty floorboards. On the wall opposite to the door, there is something that must have been a bed long ago. Next to it, a night table. There is also a cupboard, a diner table that was half-eaten by termites and a big bucket that must have been used for some very basic form of washing up." Nada
        , Room "A dark corridor" "You're walking on a creepy natural corridor. Far-away sounds, echoing through the walls, give you the creeps. You know, there is a ladder right behind you, leading to a hatch, that will allow you to leave this underground madness. I'm just saying. Nobody will be judging you if you act like a coward. I mean not everyone is cut out to be a hero, right ? Let's face it, you should be working in a cubicle, right now. Not dwelve in the heart of a Transylvanian mountain, where some monsters will most likely tear your chest apart and make a supper out of your brain.\nAnyway, the corridor continues to the south. In the darkness. With lots of creepy sounds. Not to scare you or anything." Dark]

ldObjects = [makeExit "South" [] "In front of the castle" "the gate of the castle" "The southern gate" 
            ,makeExit "East" [] "In front of the castle" "a path on the mountain" "A muddy path" 
            ,makeExit "North" [] "The southern gate" "the gate of the castle" "In front of the castle" 
            ,makeExit "West" [] "A muddy path" "an upward path to the castle" "In front of the castle" 
            ,makeDoor ["A door", "door"] "A muddy path" "the gamekeeper shack" "The gamekeeper shack" Nothing Closed 
            ,makeDoor ["A door", "door"] "The gamekeeper shack" "a way out to the path below Lambdacula's Castle" "A muddy path" Nothing Closed 
            ,simpleObject ["the book", "book", "Lady Chatterley", "Lady's Chatterley's Lover"] "The gamekeeper shack" ladyChatAction "On the night table, an old copy of Lady Chatterley's Lover."
            ,simpleObject ["the rug", "rug", "oriental touch"] "The gamekeeper shack" rugAction "Decorum is not really the place forte, though there is a nice rug on the floor."
            ,makeDoor ["A hatch", "the hatch", "hatch"] "The gamekeeper shack" "Downwards to the unknown" "A dark corridor" Nothing Hidden]



-------------------
-- Ojbect methods
-------------------
-- Reminder:
-- ROBehaviour = object -> action -> Maybe String for complex actions
-- And return a WorldAction

-- Lady Chaterley's Lover
ladyChatAction :: RoomObjectBehaviour
ladyChatAction book Use _ = singleAnswer $ "What do you want to use the book on ?"
ladyChatAction book Examine _ = singleAnswer $ "Wow, it's a very old print. It looks like it's seen better day though. The previous owner seemed to love this book a lot. Perhaps a bit too much, actually."
ladyChatAction book Take _ = pickItem book
ladyChatAction _ _ _ = singleAnswer $ "I don't know what you're trying to do with this book. And frankly, I don't want to know."

rugAction :: RoomObjectBehaviour
rugAction rug Examine _ = singleAnswer $ "OK, it's probably not a masterwork as far as tapestry is concerned, but at least they were trying."
rugAction rug Lift _ = do
                        setExternalStatus "The gamekeeper shack" "A hatch" Closed
                        return ["There is a hatch behind the rug !"]
rugAction rug _ _ = singleAnswer $ "You DO realize this is a rug, right ?"
