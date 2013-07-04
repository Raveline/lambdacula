module Lambdacula.GameData where

import Control.Monad.State
import Data.List
import Control.Lens hiding (Action)

import Lambdacula.Action
import Lambdacula.World
import Lambdacula.Display
import Lambdacula.ModelShortcuts
import qualified Data.Map as Map

type Topics = Map.Map String String
type TopicAliases = Map.Map String String

-- VERBS
verbs = [Transitive Talk "speak" ["with", "to"] ["about"] False
        ,Transitive Talk "talk" ["with", "to"] ["about"] False
        ,Transitive Talk "ask" ["about"] [] False
        ,Phrasal Search "look" "for" [] ["in", "with"] True
        ,Transitive Examine "examine" [] ["with"] False
        ,Phrasal Examine "look" "at" [] ["with"] False
        ,Transitive Examine "look" [] ["with"] False
        ,Transitive Open "open" [] [] False
        ,Transitive Examine "analyze" [] [] False
        ,Transitive Move "go" [] [] False
        ,Transitive Eat "eat" [] [] False
        ,Transitive QuitGame "quit" [] [] False
        ,Transitive Use "use" [] ["on"] False
        ,Transitive Use "open" [] ["with"] False
        ,Transitive Use "unlock" [] ["with", "using"] False
        ,Phrasal Take "pick" "up" [] ["from", "in", "on"] True
        ,Transitive Take "take" [] ["from", "of", "out"] True 
        ,Transitive Lift "lift" [] [] False
        ,Transitive Search "search" [] [] False
        ,Transitive Flee "flee" [] [] False
        ,Transitive Attack "attack" [] [] False
        ,Transitive Attack "kick" [] [] False
        ,Transitive Attack "hit" [] [] False
        ,Transitive Inventorize "inventory" [] [] False]


-- CONSTANTS
none = "NONE"
nope = singleAnswer $ "I don't think I understand what you want."

-- OBJECTS
simpleObject :: [String]                -- Names 
                -> String               -- Room
                -> RoomObjectBehaviour  -- Way it reacts
                -> String               -- Description
                -> RoomObject           -- A proper room object
simpleObject a b c d = objectContaining a b c d Nada []

objectContaining :: [String]           -- Names
                -> String               -- Room
                -> RoomObjectBehaviour  -- Way it reacs
                -> String               -- Description
                -> ObjectStatus         -- Status of the object
                -> [RoomObject]         -- Content
                -> RoomObject           -- A proper room object
objectContaining aliases room reaction description status contained = RoomObject naming room reaction details
    where
        naming = ObjectNames aliases
        details = RoomObjectDetails status description contained

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
        , Room "The castle entrance" "This is the southern part of the inner yard of the castle. Like the rest of the yard, it's but a shadow of its former glory. The yard is covered with the cobblestones. The little steps leading to the main doors of the castle are covered with cracks. As for the castle, frankly, you're amazed it has not yet collapsed. Time must have forgotten to destroy everything here." Nada
        , Room "The kitchen" "You're standing in a vast room, specifically designed to the production of fine food. Well, it used to be, because like everything else here, it has seen better days. Everything food related has been eaten by rats, insects and even less pleasant animals. Dusty or broken tableware lay on the tables. Empty, opened cupboard made out of rotting wood are waiting for a mercyful soul to chop their soul out of here. Needless to say, there is no fridge, no coffee machine, not the slightest touch of modern comfort. Making anything edible in here would be quite a feat." Nada
        , Room "The guard room" "In the Middle-Ages, this room would have been packed with tough looking men-at-arms, patrolling merrily, freezing their asses around the braseros, singing politically incorrect songs. Nowadays, it's just a big room, almost empty, though the braseros are still there. It's a big room with six heavy columns. Though the walls are sometimes cracked, you think it's not going to crumble before long, so feel free to frolick around as much as you need. Against the southern wall, you notice a narrow round staircase leading to an upper floor." Nada
        , Room "The dormitory" "This used to be a place where the guards would sleep. Arrow slits let the light from the outside shine in. There are twenty beds, and straw mattresses, or what's left of them. Though you notice one of the beds is very neatly done. There is also a little do-it-yourself hearth, with a cooking pot. And next to it... " Nada
        , Room "A gallery" "This is a long corridor used to display the portraits of the former masters of the castle. This place is not too bad. There is a ceremonial red, velevty rug on the floor, that the rats almost didn't use as their privy. Beautiful chandeliers hang from the ceiling. Of course there are no candles anymore, but little windows give you enough light to be able to look around. On the walls, sixteen paintings of the former Lords of Lambdacula and their spouses. Look, they are all here !" Nada
        , Room "Antichamber" "This is a little antichamber or vestibule. I never understood exactly why castle would have this kind of rooms. What on earth did they use it for ? Well if it had any kind of use in the old days, it has none whatsoever today. The place is just a boring old room, with light coming from dusty windows showing the inner yard of the castle. There is a mosaic on the floor, but it is so ugly I decided I'm not going to describe it. On the walls, squatters have drawn graffiti like \"No more garlic\" or \"Transylvannia rulez\". Nothing worth your time." Nada
        , Room "A muddy path" "You're walking along a path that goes down the mountain of Lambda, where the castle of Lambdacula was built. You'd like follow the path onwards and leave this dreadful place to go back to civilization but a strange feeling prevent you from... oh, who am I kidding ? You can't escape, you coward, you've got to face the count and murder him, because this is the goal of this sick, twisted, vampirophobic videogame. If you've got an issue with that, just type \"quit\" and be gone.\nWhere was I ? So, yes, the path could let you get away. The only interesting place here is a shack. It's most likely where the gamekeeper of Count Lambdacula used to live. Well, there is the slight possibility that the gamekeeper is immortal and still lives there, so you never know, that's the issue with undeads." Nada
        , Room "The gamekeeper shack" "You're inside a simple shack made out of rudimentary logs. Everything is covered with spider webs. Apparently, no one has been there for a long time, though you could swear there are some footsteps on the dusty floorboards. On the wall opposite to the door, there is something that must have been a bed long ago. Next to it, a night table. There is also a cupboard, a diner table that was half-eaten by termites and a big bucket that must have been used for some very basic form of washing up." Nada
        , Room "The dining room" "You're standing in Castle Lambdacula formal dining room. You're quite sure that even before this place was crumbling, hell, even before the Count of Lambdacula became a vampire, this room was already sinister. An enormous, wooden table reinforced with iron stands in the middle. Chairs forged from iron, with impressive and, let's be frank, monstrous creativity surround it. And I don't want to make unwanted comments about the way things are in this castle, but really, if you had to pick a dining room, would you go with the one with a terrible draught ?" Nada
        , Room "The living room" "You're standing in a vast and cosy living room. Strangely enough, this particular room is in a perfect state. The armchairs and sofa are still there, and not invaded by spiderwebs. It is as if some people used this room regularly. Well, after all, if they are vampire in this castle, it is normal that they should have a place to meet and talk together, you know." Nada
        , Room "The library" "You're standing in a nice library, perfectly preserved. The whole room is wood-paneled. There are two comfortable armchairs in the middle. From the windows, you can see the distant landscape of Transylvania. Once again, it looks like people have been using the place recently. It's nice, in this day and age, to know that there are some who still find comfort in intellectual activities, and will spend a lovely evening reading books. Well, considering the fact that most inhabitant of the castle must be vampires, they probably spend the night drinking blood of innocent bystanders, but you know, once it's done, they go to the library." Nada
        , Room "The hall" "This is the entrance hall of Lambdacula Castle. Well, it's not as grandiose as you could have expected, but it's not that bad. Particularly if you've got nothing against dark, eerie shadows or cobwebs, or the nasty feeling that someone is watching your every step. And stone, of course. You have to love stone to appreciate the place, because, except from one or two medieval armours, the only thing you're going to get here is stone. Everywhere." Nada 
        , Room "The smoking room" "This is a cosy little room, with nice furniture. It was the place where the men would go after a heavy diner, and somke the cigar and discuss matter of current politics. Or football. Or the latest xbox game. Or whatever. You notice a beautiful japanese inspired wallpaper. The windows on the east let you admire a little inner yard, with a garden." Nada
        , Room "The inner garden" "You are standing in a little inner yard, covered with flowers and dangerously untamed trees. There were once gravel alleys to walk ildly between the plants, but nature has a way to claim back everything, so I would advise against having a little stroll in this garden turned jungle." Nada
        , Room "The chapel" "This used to be the chapel of the castle, but there has been some major change, here. First of all, every crosses have been removed. How weird is that, huh ? Then it is obvious it's been a long time since anybody held a mass here. The benches are broken or knocked down." Nada
        , Room "Piano bar" "Ok, this is unexpected, but the castle has its own piano bar. Lovely place, covered with rugs. You see a nice Pleyel grand piano on the stage. Unfortunately, there are no bottle in the bar anymore. But there is a blackjack table ! With no players nor dealers, though, so, no, you're not going to get rich today." Nada
        , Room "The conservatory" "You're standing in a little greenhouse, dedicated to growing exotic plants and drinking tea. In both instance, the place fails miserably though. The plants are all dead - and dead as in long gone, not as living-dead. There are barely somme roots that survived the attacks of time, god knows how. As far as tea is concerned, there once was an ebony table, but it's been turned into a luxury restaurant for termites, and there is not much left." Nada
        -- 2ND FLOOR
        , Room "The countess bedroom" "Silk, velvet, tasteful tapestry on the wall, elegant dressing table : no doubt to have, you are in the countess bedroom. And boy, did she like purple. Everything here is purple : the wallpaper, the sheet on the bed, the rugs. You'd swear there is something wrong with your eyes. The room is very well cared for, which is quite strange come to think of it : the countess is supposed to have died five centuries ago, so you don't really see the need to do her room everyday." Nada
        , Room "The north-west corridor" "You are standing in a massive corridor. It probably served some defensive purpose a long time ago. You are above the guard room, and you can see that many openings were made to pour various burning fluids would the room beneath be conquered by assailants. The stairs that lead to the guard room are narrow and round, to make the defense easier." Nada
        , Room "The north-east corridor" "You're walking along the second floor corridor. Your steps are muffled by a thick rug that deserves a long trip to the dry-cleaner. The place is decorated without taste and with armours, armours everywhere. Why, oh why do they always feel the need to put armours everywhere in this old castles ? Don't they understand how cliché the whole thing is ? Anyway. " Nada
        , Room "The amusement arcade" "OK, this is not exactly what you expected to find here. The Count of Lambdacula must have had this place installed in the 1980s. There are great video game oldies and pinballs here, and the lovely, obseding noise of a fifteen electronic chips making their own music at the same time. There is even the smell of cigarettes in the air, just like in the old times. " Nada
        , Room "The central corridor" "As you walk through this part of the second floor corridor, you have chilly feeling going down your spine (please ignore this comment if you are a mollusk). This can only mean two things : you are close to the Count's Lair... or there is a draught (or, as they would say in the colonies, \"a draft\"). Actually, you're close to the master bedroom, judging by the opulent golden candelstick stuck on the wall. Well, vampires can be so very nouveau riche, you know." Nada
        , Room "The master bedroom" "This used to be the bedroom of the count. Well, maybe it still is, but vampires are known to prefer the cosy comfort of a coffin to the silky pleasure of a decadent bed." Nada
        , Room "The countess bathroom" "It shall not be said that the transylvanian countryside is devoid of the comfort and sophistication of the modern life. This is a fully equipped bathroom, with an elegant porcelain bathtub." Nada
        , Room "The private living room" "This little cosy room was used by the family of the former Counts of the castle when they didn't entertain guests. There are only two armchairs and a two-sitter." Nada 
        -- UNDERGROUND WORLD
        , Room "A dark corridor" "You're walking on a creepy natural corridor. Far-away sounds, echoing through the walls, give you the creeps. You know, there is a ladder right behind you, leading to a hatch, that will allow you to leave this underground madness. I'm just saying. Nobody will be judging you if you act like a coward. I mean not everyone is cut out to be a hero, right ? Let's face it, you should be working in a cubicle, right now. Not dwelve in the heart of a Transylvanian mountain, where some monsters will most likely tear your chest apart and make a supper out of your brain.\nAnyway, the corridor continues to the south. In the darkness. With lots of creepy sounds. Not to scare you or anything." Dark]

ldObjects = [makeExit ["South"] "In front of the castle" "the gate of the castle" "The southern gate" 
            ,makeExit ["East"] "In front of the castle" "a path on the mountain" "A muddy path" 
            -- Southern gate
            ,makeExit ["North"] "The southern gate" "the gate of the castle" "In front of the castle"
            ,makeExit ["South"] "The southern gate" "the rest of the yard" "The castle entrance"
            ,makeDoor ["West", "metallic door", "metal door"] "The southern gate" "A metallic door" "The guard room" Nothing Closed
            ,makeDoor ["East", "wooden door", "wooden door"] "The southern gate" "A wooden door" "The kitchen" Nothing Closed
            ,objectContaining ["pile", "pile of junk", "junk", "refuse"] "The southern gate" junkPileAction "There is a pile of junk next to the gates of the castle, and if you stay there too long, I'd say a pool of vomit close to it." Opened [simpleObject ["a torchlight", "torchlight", "torch"] "NOWHERE" torchlightAction "A torchlight"] 
            -- Castle entrance
            ,makeExit ["North"] "The castle entrance" "To the southern gate" "The southern gate"
            ,makeDoor ["South", "double doors", "doors", "door"] "The castle entrance" "An impressive double doors" "The hall" Nothing Closed
            -- Kitchen
            ,makeDoor ["West", "wooden door", "wooden door"] "The kitchen" "A wooden door" "The southern gate" Nothing Closed
            ,makeExit ["South"] "The kitchen" "To an antichamber" "Antichamber"
            ,objectContaining ["cupboard", "cupboards"] "The kitchen" cupboardAction "You notice one cupboard that seem in better state than the others." Closed 
                [simpleObject ["saucepan", "pan"] "" saucepanAction "A saucepan"
                ,simpleObject ["salt", "saltcellar", "salt cellar", "saltshaker"] "" saltAction "A saltshaker"
                ]
            ,simpleObject ["cat bowl", "bowl"] "The kitchen" catBowlAction "On the floor, there seem to be a bowl with some water in it, for a cat. Or a dog. Whatever it is, it's not around anyway."
            ,simpleObject ["oven", "old oven"] "The kitchen" ovenAction "The old oven is still there, a tribute to the quality of the craftsmanship of yore."
            -- Guard room
            ,makeDoor ["East", "metallic door", "metal door"] "The guard room" "A metallic door" "The southern gate" Nothing Closed
            ,makeExit ["West", "wooden door", "wooden door"] "The guard room" "An opened door" "The dormitory"
            ,makeExit ["South"] "The guard room" "An impressive corridor" "A gallery" 
            ,makeExit ["Up", "upstairs"] "The guard room" "Stairs leading to the second floor" "The north-west corridor"
            ,simpleObject ["brasero"] "The guard room" braseroAction "One of the braseros seems in perfect condition."
            -- Dormitory
            ,makeExit ["East", "wooden door", "wooden door"] "The dormitory" "An opened door" "The guard room" 
            ,simpleObject ["Sad", "man", "goth", "necromancer", "necro", "man in robes", "man in black robes"] "The dormitory" necroAction "there is a man in black robes, wearing make up to look as pale as possible. He's got skull-shaped rings on almost all his fingers."
            -- Gallery
            ,makeExit ["North"] "A gallery" "To the guard room" "The guard room"
            ,makeExit ["South"] "A gallery" "To the living room" "The living room"
            ,simpleObject ["portrait of Stanislas of Lambdacula", "portrait", "Stanislas", "Stanislas of Lambdacula"] "A gallery" stanislasAction "A portrait of Stanislas of Lambdacula !"
            ,simpleObject ["portrait of Dolores of Lambdacula", "portrait", "Dolores", "Dolores of Lambdacula"] "A gallery" doloresAction "A portrait of Dolores of Lambdacula !"
            ,simpleObject ["portrait of Igor of Lambdacula", "portrait", "Igor", "Igor of Lambdacula"] "A gallery" igorAction "A portrait of Igor of Lambdacula !"
            ,simpleObject ["portrait of Sveltana of Lambdacula", "portrait", "Sveltana", "Sveltana of Lambdacula"] "A gallery" sveltanaAction "A portrait of Sveltana of Lambdacula !"
            ,simpleObject ["portrait of a man without a name", "portrait", "unnamed portrait", "anonymous portrait", "portrait without a name", "nameless man portrait", "portrait of a nameless man"] "A gallery" anonymousPortraitAction "A portrait of a man without a name !"
            -- Antichamber
            , makeExit ["North"] "Antichamber" "To the kitchen" "The kitchen"
            , makeExit ["East"] "Antichamber" "To the dining room" "The dining room"
            , makeExit ["South"] "Antichamber" "To the smoking room" "The smoking room"
            , simpleObject ["soda vending machine", "vending machine", "machine"] "Antichamber" vendingMachineAction "Against one wall, there is a slightly incongruous vending machine."
            -- Dining room
            , makeExit ["West"] "Dining room" "To the antichamber" "The antichamber"
            , objectContaining ["dresser"] "Dining room" dresserAction "On the wall opposite to the door, there is a massive dresser to store crockery." Opened
            [simpleObject ["A vial of emetic", "drugs", "vial of emetic", "emetic", "vial"] "" emeticAction "A vial that, according to its label, contains a potent emetic. Don't ask me what it is doing there."
            ,simpleObject ["fork"] "" forkAction "A simple fork. That's all there's left of all the cutelry and crockery that must have been stored here once."]
            -- Living room
            , makeExit ["North"] "The living room" "To the gallery" "A gallery"
            , makeExit ["West"] "The living room" "To the library" "The library"
            , makeExit ["East"] "The living room" "To the hall" "The hall"
            -- The library
            , makeExit ["East"] "The library" "To the living room" "The living room"
            -- The hall
            , makeExit ["West"] "The hall" "To the living room" "The living room"
            , makeDoor ["North", "double doors", "doors", "door"] "The hall" "An impressive double doors" "The castle entrance" Nothing Closed
            , makeExit ["East"] "The hall" "To the smoking room" "The smoking room"
            -- The smoking room
            , makeExit ["East"] "The smoking room" "To an inner garden" "The inner garden"
            , makeExit ["North"] "The smoking room" "To an antichamber" "The antichamber"
            , makeExit ["West"] "The smoking room" "To the hall" "The hall"
            -- Inner garden
            , makeExit ["West"] "The inner garden" "To the smoking room" "The smoking room"
            , makeExit ["South"] "The inner garden" "To the conservatory" "The conversatory"
            -- The northwest corridor
            , makeExit ["Down", "downstairs"] "The north-west corridor" "Stairs to the first floor" "The guard room"
            , makeDoor ["West", "wooden door", "door"] "The north-west corridor" "A wooden door" "The countess bedroom" Nothing Closed
            -- The countess bedroom
            , makeDoor ["East", "wooden door"] "The countess bedroom" "A wooden door" "The north-west corridor" Nothing Closed
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

-- THE SHACK
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

-- THE SOUTHERN YARD
junkPileAction :: RoomObjectBehaviour
junkPileAction junk Examine _ = singleAnswer $ "It is your classical junk pile, fully equipped with rags, bits of wood and metal, organic matter we do not want to know more about, most likely a rat nest. Who am I kidding ? A rat kingdom. With counties and all."
junkPileAction junk Search _
    | length (view containedObjects junk) > 0 = singleAnswer $ "The only think you're going to get from this is a severe case of tetanos. Well. Your funeral. You only find one interesting thing in the pile : a torchlight. Most likely broken. I would not pick it up if I were you."
    | otherwise = singleAnswer $ "Look, you already managed to pick up a torchlight from this thing. And you survived. You should be grateful, and not playing with your life again."
junkPileAction junk Take (Just x) = pickItemFromContainer junk x
junkPileAction _ _ _ = singleAnswer $ "There is a limited amount of things you can do with a junk pile. Fortunately."

torchlightAction :: RoomObjectBehaviour
torchlightAction torch Use (Just "battery") = do
                                                changeStatus torch Powered
                                                return ["Great, you now have a functioning torchlight. This should shed some light on your current issues. Shed some light... got it ? HAHAHAHA. Ahem. Sorry."]
torchlightAction torch Use _ 
    | view objectStatus torch == Powered = do
                                            changeStatus torch Luminescent
                                            return ["You turn the torchlight on. You now have a lightsource and can go in dark places !"]
    | view objectStatus torch == Luminescent = do
                                                changeStatus torch Powered
                                                return ["You turn the torchligts off. You have no lightsource now. Avoid dark places !"]
    | otherwise = singleAnswer $ "There are no batteries in the torchlight !"
torchlightAction torch Examine _
    | view objectStatus torch == Powered = singleAnswer $ "A torchlight in almost perfect condition."
    | view objectStatus torch == Luminescent = singleAnswer $ "The torchlight is on. Looking at it would blind you. You're not very bright, are you ? HAHAHAHAHA, GOT YOU AGAIN !"
    | otherwise = singleAnswer $ "Well, it doesn't look like it's broken... but there are no battery inside. So, you know. It's not going to work. But it could. Well, maybe. Most likely, the rats ate the wire inside of this thing. I'm just saying, you know, don't be too hopeful."

-- THE GUARD ROOM
braseroAction :: RoomObjectBehaviour
braseroAction brasero Use (Just "mysterious paper") = error "TODO"
braseroAction brasero Use _ 
    | view objectStatus brasero == Luminescent = singleAnswer $ "What do you want to use the brasero for ?"
    | brasero `containsSomethingNamed` "coal" = singleAnswer $ "There is some coal in the brasero. If you have a lighter or a match, you can try to start a fire."
    | otherwise = singleAnswer $ "The brasero is not lit. You need some kind of fuel to light it."
braseroAction brasero examine _
    | view objectStatus brasero == Luminescent = singleAnswer $ "There is a nice fire in the brasero to warm you up."
    | otherwise = singleAnswer $ "The brasero is not lit. If you had any kind of fuel and something to start a fire, you could probably use it though." 

-- THE KITCHEN
cupboardAction cupboard Examine _
    | view objectStatus cupboard == Closed = singleAnswer $ "It's a closed cupboard."
    | view objectStatus cupboard == Opened = lookInsideContainer cupboard
cupboardAction cupboard Open _ = openContainer cupboard "You open the cupboard"
cupboardAction cupboard Search _ = cupboardAction cupboard Examine Nothing
cupboardAction cupboard Take (Just x) = pickItemFromContainer cupboard x
cupboardAction cupboard Take _ = singleAnswer "It's heavy. You're weak. And anyway, it's a stupid idea. No offense."
cupboardAction _ _ _ = nope

ovenAction oven Examine _ = singleAnswer "A wonderful old oven, as they used to do them in the old days. It works a bit like a fireplace. You could use it to cook anything. Even meth, I think. Don't do it, though. Meth is bad. I didn't just encourage you to do such a thing."
ovenAction oven Use _ = singleAnswer "You need to put wood and start a fire"

catBowlAction bowl Use (Just "Zinc bit") = putInsideContainer bowl "Zinc bit" "You've put the zinc in the bowl."
catBowlAction bowl Use (Just "Copper bit") = putInsideContainer bowl "Copper bit" "You've put the copper in the bowl."
catBowlAction bowl Use (Just "Electric wires") 
    | numberOfContained bowl == 2 && view objectStatus bowl == Salted  = 
            putInsideContainer bowl "Electric wires" "You plug the wires to the zinc and copper bits. You did it ! You know have a source of electricity that would make Tesla proud !"
    | otherwise = singleAnswer $ "No, something is still missing, there."
catBowlAction bowl Use (Just "salt") = do
                                    changeStatus bowl Salted
                                    return ["You put salt in the water. That's not very nice for the pet that'll try to drink it, you know."]
catBowlAction bowl Use _ = singleAnswer "Look, you might be thirsty, but not THAT thirsty."
catBowlAction bowl Take _ = singleAnswer "And deprive a cat, dog, or monstrous vampiric pet of his drink ? NOT ON MY WATCH !"
catBowlAction bowl Examine _ = ifContainsDo bowl (Map.fromList [(0, singleAnswer "Just normal water in a bowl. Nothing to write home about.")
                                                        ,(1, singleAnswer "A bowl with a bit of metal inside. How weird !")
                                                        ,(2, singleAnswer "A bowl with one bit of zinc and one bit of copper inside. This is weird indeed !")
                                                        ,(3, singleAnswer "A bowl with two bits of metal and electric wire connected to them... OK. OK.")])
catBowlAction bowl Talk _ = singleAnswer $ "You complain to the bowl for a while about how hard your life is, what with your tendency to speak to inanimous objects and all."
catBowlAction _ _ _ = nope

saltAction salt Examine _ = singleAnswer $ "It's a saltshaker. With salt inside. I WOULD give you the name of the brand, but they refused to sponsor this game, so you know. I won't."
saltAction salt Take _ = pickItem salt
saltAction salt Use _ = singleAnswer $ "Use it how ? To throw behind your shoulder ? OH COME ON, don't tell me you believe this ? OK, vampire, werewolf and zombies do exist in this game, but fairies ? Please."
saltAction salt Eat _ = singleAnswer $ "It wouldn't be good for your arteries. I'm doing you favour."
saltAction salt Talk _ = singleAnswer $ "The salt is not answering. Ever considered having your mental health tested ? Just asking."
saltAction _ _ _ = nope

saucepanAction pan Take _ = pickItem pan
saucepanAction pan Examine _ = singleAnswer $ "Well, it's a saucepan. It's been used before, but still ready to boil stuff if neeeded."
saucepanAction _ _ _ = nope

-- DORMITORY
necroAction :: RoomObjectBehaviour
necroAction necro Examine _ = singleAnswer $ "Are we really going to have to spend time considering this emo character ? OK, obviously he is not a vampire. Most likely, he would love to be one. In any case, he's going to make you waste your time. And be annoying. Don't tell me I haven't warned you."
necroAction necro Talk Nothing = necroAction necro Talk (Just "hello")
necroAction necro Talk (Just word) = conversation word [("hello", ["hi"]), ("sad", ["sadness", "being sad", "him being sad"]), ("mother", ["mum", "mummy"]), ("happy", []), ("necromancer", ["necro", "necromancy"]), ("dissertation", ["essay", "topic"]), ("death", []), ("taxes", ["tax"]), ("count", ["count Lambdacula"]),("crypt", ["secret crypt"]), ("vampire", ["vampiric society", "vampires", "vampire society"]), ("Transylvania", []), ("zombies", ["zombie"])] [("hello", "Hellooooo dude. I'm sad.")
        ,("sad", "No, no, I'm pretty happy. My NAME is Sad is all. You know. My mother was kinda depressed when she had me. It was supposed to be just an ordinary case of baby blues, but actually, it never stopped. I think she's better now I've left her to become a Necromancer.")
        ,("mother", "My mother was a tailor, my father was a gambler... you know how it goes.")
        ,("happy", "Look, man, this is Lambdacula Castle. There is no BETTER PLACE to be for a Necromancer.")
        ,("necromancer", "Yes, that's my major. I wanted to major in arts and stuff, but in this economy... Anyway, it turned out to be a passion, so I'm doing my PhD in Necromancy. I'm doing this trip here to collect data for my dissertation.")
        ,("dissertation", "What's my dissertation about ? \"Adoration of Death and Cult of Decline amongst Transylvanian Vampiric Communities from a Postmodern Point Of View\". I know, the title is too short, it doesn't look serious...")
        ,("death",  "Look, dude, I spend my WHOLE DAY talking, reading and writing about death, I'd like to speak about something else. Like taxes, for instance. Taxes are fun.")
        ,("taxes", "Did you know, for instance, that Count Lambdacula is a very law-abiding citizen ? He pays his taxes every year. Well, that's what I've heard.")
        ,("count", "The count ? I've never met him. I hope to. The rumour is he's sleeping in a secret crypt somewhere in the castle.")
        ,("crypt", "Well it's been fashionable to have a secret crypt in the vampire society for quite a long time, you know. I have no idea where it is though.")
        ,("vampire", "Vampires are living dead who drink blood. I think that you should know that before coming to Transylvania, you know.")
        ,("Transylvania", "Well, it's a nice place, you know. Particularly if you like vampires. Which I do. I mean, come on, I know some people specialize on zombies, but, seriously, ZOMBIES ? Walking rotting corpses with no brain whatsoever ? That's sick, man.")
        ,("zombies", "I think they are some around, but frankly, my dear, I don't give a damn.")
        ,("NONE", "I have nothing to say about that.")] 

-- Gallery
stanislasAction :: RoomObjectBehaviour
stanislasAction portrait Examine _ = singleAnswer $ "A portrait of Stanilas Lambdacula, count of the castle between 1357 and 1402. A fine looking fellow, actually."
stanislasAction portrait Take _ = singleAnswer $ "You're not a burglar. Besides, you don't want to carry this thing with you all the time."
stanislasAction _ _ _ = singleAnswer $ "You can't do much with this portrait."

doloresAction :: RoomObjectBehaviour
doloresAction portrait Examine _ = singleAnswer $ "A portrait of Dolores Lambdacula, countess of the castle. Born in 1369, She married Stanislas of Lambdacula in 1385 and ruled till her son was old enough, in 1408. She died in 1423. Wait a second... Dolores doesn't sound like very Roumanian ! She must have come from Spain or something."
doloresAction portrait Take _ = singleAnswer $ "You're not a burglar. Besides, you don't want to carry this thing with you all the time."
doloresAction _ _ _ = singleAnswer $ "You can't do much with this portrait."

igorAction :: RoomObjectBehaviour
igorAction portrait Examine _ = singleAnswer $ "A portrait of Count Igor Lambdacula, son of Stanislas and Dolores, born in 1392. He ruled from 1408 to his death in 1459. Igor didn't look like a very fun guy, considering how dak and brooding he looks on this painting."
igorAction portrait Take _ = singleAnswer $ "You're not a burglar. Besides, you don't want to carry this thing with you all the time."
igorAction _ _ _ = singleAnswer $ "You can't do much with this portrait."

sveltanaAction :: RoomObjectBehaviour
sveltanaAction portrait Examine _ = singleAnswer $ "A portrait of Countess Sveltana Lambdacula, wife of Igor Lambdacula, born in 1397. She married him in 1412. Truth be told, she was quite beautiful. She died in 1453."
sveltanaAction portrait Take _ = singleAnswer $ "You're not a burglar. Besides, you don't want to carry this thing with you all the time."
sveltanaAction _ _ _ = singleAnswer $ "You can't do much with this portrait."

anonymousPortraitAction :: RoomObjectBehaviour
anonymousPortraitAction portrait Examine _ = accordingToStatus portrait (Map.fromList[(Nada, singleAnswer $ "It's weird, there is no name below this portrait. Must be one of the Counts that ruled over the castle, but no informations about him. He's quite dashing. A bit pale, though."), (Fixed, singleAnswer $ "You dropped white spirit on the portrait, embracing your inner call towards vandalism. Since then, one can read on the bottom right side : \"Count Vlad Lambdacula, born in 1418.\".")])
anonymousPortraitAction portrait Take _ = singleAnswer $ "No ! It's too big. And it's not yours to take."
anonymousPortraitAction portrait Use (Just "white spirit") = singleAnswer $ "Oblivious of the fact you are most likely desacrating an historical treasure, you pour white spirit on the painting. You manage to scrap the bottom right corner of the canvas. There is something written here ! \"Count Vlad Lambdacula, born in 1418.\" Hmm... no date of death. Could it be... ?"
anonymousPortraitAction _ _ _ = singleAnswer $ "I don't want to know how you got this idea. Not going to work."

-- Antichamber
vendingMachineAction :: RoomObjectBehaviour
vendingMachineAction machine Examine _ = singleAnswer $ "Well, it's a soda vending machine that looked like it was tuned into a cocktail vending machine. But in only serves bloody mary according to the labels on the buttons. Weird. Anyway, the machine is not plugged. And there doesn't even seem to be anything to plug it, so... forget your bloody mary."
vendingMachineAction machine Attack (Just "crowbar") = do
                                            addToInventory $ simpleObject ["coin"] "" coinAction "A coin"
                                            removeFromInventoryByName "crowbar"
                                            return ["Ha ! I like a thug with some method. Ok, you apply the crowbar on the machine. After a loud \"clang\", you manage to brreak the crowbar, so you won't be able to use it anymore. But there are good news : ONE coin fell on the ground. You pick it up quickly. You are now the proud owner of a coin !"]
vendingMachineAction machine Attack _ = singleAnswer $ "There is nothing like gratuitous violence. However, as you try to kick the machine with your foot, you hurt yourself. If you want to continue this aggressive behaviour, you are most likely going to need help. Come back with a gang of baddies. Or with some kind of weapon."
vendingMachineAction _ _ _ = singleAnswer $ "I won't let you do stupid things to a vending machine. Tempting as it might be."

-- Dining room

dresserAction :: RoomObjectBehaviour
dresserAction dresser Examine _ = accordingToStatus dresser (Map.fromList[(Closed, singleAnswer $ "It is a massive, walnut-made dresser with glass paned doors. Though the glass is very, very dusty you can see through it that there is almost nothing to see inside. You'd probably could check a bit more if you opened it."), (Opened, lookInsideContainer dresser)])
dresserAction dresser Take (Just x) = pickItemFromContainer dresser x
dresserAction dresser Open _ = openContainer dresser "You open the dresser, and in a cloud of dust, you manage to check the inside."
dresserAction _ _ _ = singleAnswer $ "The dresser was most likely not designed for this kind of use."

emeticAction emetic Examine _ = singleAnswer $ "It's an emetic, namely something used to make you vomit."
emeticAction emetic Eat _ = singleAnswer $ "No, you're not anorexic. And things are already nauseating enough."
emeticAction _ _ _ = singleAnswer $ "The less you interact with this stuff, the better you'll feel."

forkAction :: RoomObjectBehaviour
forkAction fork Examine _ = singleAnswer $ "It is a metallic fork. Really nothing special about it."
forkAction fork Eat _ = singleAnswer $ "No, no, no, you do not EAT FORKS. You USE THEM TO EAT. I know, it's a lot of stuff to understand. But I'm sure you'll manage."
forkAction _ _ _ = singleAnswer $ "One player. One fork. Not a whole lot of possibilities, there."

-- various inventory stuff
coinAction :: RoomObjectBehaviour
coinAction coin Examine _ = singleAnswer "Some romanian coin. Let's face it : you didn't take time to study the local currencies. Me neither. So neither you nor me have the slightest idea how much it's worth. Most likely not a lot."
coinAction _ _ _ = singleAnswer $ "No, no, no, you don't want to anything that would make you risk this coin."


