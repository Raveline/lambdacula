module Lambdacula.GameData where

import Control.Monad.State
import qualified Data.Map as Map
import Data.List
import Control.Lens hiding (Action)

import Lambdacula.Action
import Lambdacula.World
import Lambdacula.Display
import Lambdacula.ModelShortcuts

type Topics = Map.Map String String
type TopicAliases = Map.Map String String

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
        ,Transitive Search "search" [] []
        ,Transitive Inventorize "inventory" [] []]


-- CONSTANTS
none = "NONE"

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


makeExit :: [String]     -- Aliases
            -> String       -- Room where this exit should be
            -> String       -- Description of the object
            -> String       -- Destination
            -> RoomObject   -- An Exit
makeExit aliases inRoom description room = Exit (ObjectNames aliases) inRoom (basicMove room) (RoomObjectDetails Opened description []) room 

makeDoor :: [String]     -- Aliases
            -> String       -- Room where this exit should be
            -> String       -- Description of the object
            -> String       -- Destination
            -> Maybe String -- Key ?
            -> ObjectStatus -- Opened / Closed / Hidden ?
            -> RoomObject   -- An Exit
makeDoor alias inRoom description room key status = Exit (ObjectNames alias) inRoom (moveDoor key room) (RoomObjectDetails status description []) room

conversation :: String                  -- Word
            -> [(String, [String])]     -- Aliases
            -> [(String, String)]       -- Conversations
            -> State World [String]     -- Reaction 
conversation subject aliases topics = case properAnswer of 
                                        Just x -> singleAnswer $ '"':(x ++ "\"")
                                        Nothing -> error $ "There should be a " ++ none ++ " topic." 
    where 
        properAnswer = case properTopic of
                        Just x -> Map.lookup x (Map.fromList topics)
                        Nothing -> Map.lookup none (Map.fromList topics)
        properTopic = Map.lookup subject mapAliases
        mapAliases = aliasToMap aliases
        
aliasToMap :: [(String, [String])] -> TopicAliases
aliasToMap xs = Map.fromList [(alias, key)|(key, aliases)<- xs, alias <- key:aliases]  

ldRooms = [Room "In front of the castle" "You're standing in front of the castle of Lambdacula, in the heart of transylvania. It is standing at the top of a moutain, as any proper gothic castle should be. In front of you, to the south, the gates of the castle lead to the inner yard. I could describe the howling wind, the eerie atmosphere, the uncanny mist, the noise of flapping bats and other items from my Dictionnary Of Transylvanian Clichés, but I think you've got the idea. To the south, you'll find the gate of the castle, that you can cross to enter into an inner yard. On the east, a little path should lead you to safety or towards new adventures, but, come on, try to finish this one first." Nada
        -- FIRST FLOOR.
        , Room "The southern gate" "This is the inner yard of the castle. Obviously, count Lambdacula must have financial trouble, or his skills in household management are more than lacking. The place is a wreck, let's face it. There is an awful stench everywhere, rats are running through the place, the windows are dusty and frankly, UNESCO World Heritage Centre would be appalled by this place." Nada
        , Room "The castle entrance" "This is the souther part of the inner yard of the castle. As the rest of the yard, it's but a shadow of its former glory. Cobblestones are laying on the ground. The little stair leading to the main doors of the castle are covered with cracks. As for the castle, frankly, you're amazed it has not yet collapsed. Time must have forgotten to destroy everything here." Nada
        , Room "The kitchen" "You're standing in a vast room, specifically designed to the production of fine food. Well, it used to be, because like everything else here, it has seen better days. Everything food related has been eaten by rats, insects and even less pleasant animals. Dusty or broken tableware lay on the tables. Empty, opened cupboard made out of rotting wood are waiting for a mercyful soul to chop their soul out of here. Needless to say, there is no fridge, no coffee machine, not the slightest touch of modern comfort. Making anything edible in here would be quite a feat." Nada
        , Room "The guard room" "In the middle age, this room would have been packed with tough looking men-at-arms, patrolling merrily, freezing their asses around the braseros, singing politically uncorrect songs. Nowadays, it's just a big room, almost empty, though the braseros are still there. It's a big room with six heavy columns. Though the walls are sometimes cracked, you think it's not going to crumble before long, so feel free to frolick around as much as you need." Nada
        , Room "The dormitory" "This used to be a place where the guards would sleep. Arrowlet vaguely let the outer light enter. There are twenty beds, and straw mattress, or what's left of them. Though you notice one of the bed is very neatly done. There is also a little do-it-yourself hearth, with a cooking pot. And next to it... " Nada
        , Room "A gallery" "This is a long corridor used to display the portraits of the former master of the castle. This place is not too bad. There is a ceremonial red, velevty rug on the floor, that the rats almost didn't use as their privy. Beautiful chandelier hang from the ceiling. Of course there is no candle anymore, but little windows give you enough light to be able to look around. On the walls, sixteen paintings of the former Lords of Lambdacula and their spouses. Look, they are all here !" Nada
        , Room "Antichamber" "This is a little antichamber or vestibule. I never understood exactly why castle would have this kind of rooms. What on earth did they use it for ? Well if it had any kind of use in the old days, it has none whatsoever today. The place is just a boring old room, with light coming from dusty windows showing the inner yard of the castle. There is a mosaic on the floor, but it is so ugly I decided I'm not going to describe it. On the walls, squatters have drawn graffiti like \"No more garlic\" or \"Transylvannia rulez\". Nothing worth your time." Nada
        , Room "A muddy path" "You're walking along a path that goes down the mountain of Lambda, where the castle of Lambdacula was built. You'd like follow the path onwards and leave this dreadful place to go back to civilization but a strange feeling prevent you from... oh, who am I kidding ? You can't escape, you coward, you've got to face the count and murder him, because this is the goal of this sick, twisted, vampirophobic videogame. If you've got an issue with that, just type \"quit\" and be gone.\nWhere was I ? So, yes, the path could let you get away. The only interesting place here is a shack. It's most likely where the gamekeeper of Count Lambdacula used to live. Well, there is the slight possibility that the gamekeeper is immortal and still lives there, so you never know, that's the issue with undeads." Nada
        , Room "The gamekeeper shack" "You're inside a simple shack made out of rudimentary logs. Everything is covered with spider webs. Apparently, no one has been there for a long time, though you could swear there are some footsteps on the dusty floorboards. On the wall opposite to the door, there is something that must have been a bed long ago. Next to it, a night table. There is also a cupboard, a diner table that was half-eaten by termites and a big bucket that must have been used for some very basic form of washing up." Nada
        , Room "The dining room" "You're standing in Castle Lambdacula formal dining room. You're quite sure that even before this place was crumbling, hell, even before the Count of Lambdacula became a vampire, this room was already sinister. An enormous, wooden table reinforced with iron stands in the middle. Chairs forged from iron, with impressive and, let's be frank, monstrous creativity surround it. And I don't want to make unwanted comments about the way things are in this castle, but really, if you had to pick a dining room, would you go with the one with a terrible draught ?" Nada
        , Room "The living room" "You're standing in a vast and cosy living room. Strangely enough, this particular room is in perfect state. The armchairs and sofa are still there, and not invaded by spiderwebs. It is as if some people used this room regularly. Well, after all, if they are vampire in this castle, it is normal that they should have a place to meet and talk together, you know." Nada
        , Room "The library" "You're standing in a nice library, perfectly preserved. The whole room is wood-paneled. There are two comfortable armchairs in the middle. From the windows, you can see the distant landscape of Transylvania. Once again, it looks like people have been using the place recently. It's nice, in this day and age, to know that there are some who still find comfort in intellectual activities, and will spend a lovely evening reading books. Well, considering the fact that most inhabitant of the castle must be vampires, they probably spend the night drinking blood of innocent bystanders, but you know, once it's done, they go to the library." Nada
        -- UNDERGROUND WORLD
        , Room "A dark corridor" "You're walking on a creepy natural corridor. Far-away sounds, echoing through the walls, give you the creeps. You know, there is a ladder right behind you, leading to a hatch, that will allow you to leave this underground madness. I'm just saying. Nobody will be judging you if you act like a coward. I mean not everyone is cut out to be a hero, right ? Let's face it, you should be working in a cubicle, right now. Not dwelve in the heart of a Transylvanian mountain, where some monsters will most likely tear your chest apart and make a supper out of your brain.\nAnyway, the corridor continues to the south. In the darkness. With lots of creepy sounds. Not to scare you or anything." Dark]

ldObjects = [makeExit ["South"] "In front of the castle" "the gate of the castle" "The southern gate" 
            ,makeExit ["East"] "In front of the castle" "a path on the mountain" "A muddy path" 
            -- Southern gate
            ,makeExit ["North"] "The southern gate" "the gate of the castle" "In front of the castle"
            ,makeExit ["South"] "The southern gate" "the rest of the yard" "The castle entrance"
            ,makeDoor ["West", "metallic door", "metal door"] "The southern gate" "A metallic door" "The guard room" Nothing Closed
            ,makeDoor ["East", "wooden door", "wooden door"] "The southern gate" "A wooden door" "The kitchen" Nothing Closed
            ,simpleObject ["pile", "pile of junk", "junk", "refuse"] "The southern gate" junkPileAction "There is a pile of junk next to the gates of the castle, and if you stay there too long, I'd say a pool of vomit close to it." 
            -- Castle entrance
            ,makeExit ["North"] "The castle entrance" "To the southern gate" "The southern gate"
            -- Kitchen
            ,makeDoor ["West", "wooden door", "wooden door"] "The kitchen" "A wooden door" "The southern gate" Nothing Closed
            ,makeExit ["South"] "The kitchen" "To an antichamber" "Antichamber"
            -- Guard room
            ,makeDoor ["East", "metallic door", "metal door"] "The guard room" "A metallic door" "The southern gate" Nothing Closed
            ,makeExit ["West", "wooden door", "wooden door"] "The guard room" "An opened door" "The dormitory"
            ,makeExit ["South"] "The guard room" "An impressive corridor" "A gallery" 
            -- Dormitory
            ,makeExit ["East", "wooden door", "wooden door"] "The dormitory" "An opened door" "The guard room" 
            ,simpleObject ["Sad", "man", "goth", "necromancer", "necro", "man in robes", "man in black robes"] "The dormitory" necroAction "there is a man in black robes, wearing make up to look as pale as possible. He's got skull-shaped rings on almost all his fingers."
            -- Gallery
            ,makeExit ["North"] "A gallery" "To the guard room" "The guard room"
            -- Antichamber
            , makeExit ["North"] "Antichamber" "To the kitchen" "The kitchen"
            , makeExit ["East"] "Antichamber" "To the dining room" "The dining room"
            -- Dining room
            , makeExit ["West"] "Dining room" "To the antichamber" "The antichamber"
            -- Muddy path
            ,makeExit ["West"] "A muddy path" "an upward path to the castle" "In front of the castle" 
            ,makeDoor ["A door", "door"] "A muddy path" "the gamekeeper shack" "The gamekeeper shack" Nothing Closed 
            -- Gamekeeper shack
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

junkPileAction :: RoomObjectBehaviour
junkPileAction junk Examine _ = singleAnswer $ "It is your classical junk pile, fully equipped with rags, bits of wood and metal, organic matter we do not want to know more about, most likely a rat nest. Who am I kidding ? A rat kingdom. With counties and all."
junkPileAction junk Search _ = singleAnswer $ "The only think you're going to get from this is a severe case of tetanos. Well. Your funeral. You only find one interesting thing in the pile : a box of match. Maybe it's empty. Maybe it's not. Life is full of surprises."

necroAction :: RoomObjectBehaviour
necroAction necro Examine _ = singleAnswer $ "Are we really going to have to spend time considering this emo character ? OK, obviously he is not a vampire. Most likely, he would love to be one. In any case, he's going to make you waste your time. And be annoying. Don't tell me I haven't warned you."
necroAction necro Talk Nothing = necroAction necro Talk (Just "hello")
necroAction necro Talk (Just word) = conversation word [("hello", ["hi"]), ("sad", []), ("mother", ["mum", "mummy"]), ("happy", []), ("necromancer", ["necro", "necromancy"]), ("dissertation", ["essay", "topic"]), ("death", []), ("taxes", ["tax"])] [("hello", "Hellooooo dude. I'm sad.")
        ,("sad", "No, no, I'm pretty happy. My NAME is Sad is all. You know. My mother was kinda depressed when she had me. It was supposed to be just an ordinary case of baby blues, but actually, it never stopped. I think she's better now I've left her to become a Necromancer.")
        ,("mother", "My mother was a tailor, my father was a gambler... you know how it goes.")
        ,("happy", "Look, man, this is Lambdacula Castle. There is no BETTER PLACE to be for a Necromancer.")
        ,("necromancer", "Yes, that's my major. I wanted to do it in art and stuff, but in this economy... Anyway, it turned out to be a passion, so I'm doing my PhD in Necromancy. I'm doing this trip here to collect data for my dissertation.")
        ,("dissertation", "What's my dissertation about ? \"Adoration of Death and Cult of Decline amongst Transylvanian Vampiric Communities from a Postmodern Point Of View\". I know, the title is too short, it doesn't look serious...")
        ,("death",  "Look, dude, I spend my WHOLE DAY talking, reading and writing about death, I'd like to speak about something else. Like taxes, for instance. Taxes are fun.")
        ,("taxes", "Did you know, for instance, that Count Lambdacula is a model citizen ? He pays his taxes every year. Well, that's what I've heard.")
        ,("count", "The count ? I've never met him. I hope to.")
        ,("NONE", "I have nothing to say about that.")] 
