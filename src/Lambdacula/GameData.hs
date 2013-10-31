module Lambdacula.GameData where

import Control.Monad.State
import Data.List
import Control.Lens hiding (Action)

import Lambdacula.Action
import Lambdacula.World
import Lambdacula.Display
import Lambdacula.Reactions
import qualified Data.Map as Map

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
        ,Transitive Push "push" [] [] False
        ,Transitive Give "give" [] ["to"] True
        ,Transitive Inventorize "inventory" [] [] False]

-- CONSTANTS
none = "NONE"

-- Lambdacula reaction set
ldreactions = [("Lady's Chatterley's Lover", Examine, Nothing, [], [Display "Wow, it's a very old print. It looks like it's seen better day though. The previous owner seemed to love this book a lot. Perhaps a bit too much, actually."])
    ,("Lady's Chatterley's Lover", Take, Nothing, [], [PickItem "Lady's Chatterley's Lover", Display "You take this wicked, naughty book. Like I'm surprised."])
    ,("Lady's Chatterley's Lover", Zilch, Nothing, [], [Display "I don't know what you're trying to do with this book. And frankly, I don't want to know."])
    ,("rug", Examine, Nothing, [], [Display "OK, it's probably not a masterwork as far as tapestry is concerned, but at least they were trying."])
    ,("rug", Lift, Nothing, [], [ChangeStatus "A hatch" Closed, Display "There is a hatch behind the rug !"])
    ,("rug", Zilch, Nothing, [], [Display "You DO realize this is a rug, right ?"])
    ,("pile", Examine, Nothing, [], [Display "It is your classical junk pile, fully equipped with rags, bits of wood and metal, organic matter we do not want to know more about, most likely a rat nest. Who am I kidding ? A rat kingdom. With counties and all."])
    ,("pile", Search, Nothing, [ContainsAmountOfItem (== 1)], [Display "The only think you're going to get from this is a severe case of tetanos. Well. Your funeral. You only find one interesting thing in the pile : a torchlight. Most likely broken. I would not pick it up if I were you."])
    ,("pile", Search, Nothing, [ContainsAmountOfItem (== 0)], [Display "Look, you already managed to pick up a torchlight from this thing. And you survived. You should be grateful, and not playing with your life again."])
    ,("pile", Take, Just "torchlight", [], [PickFromContainer "junk" "torchlight", Display "You gather enough courage to pick up the torchlight from the junk pile."])
    ,("pile", Zilch, Nothing, [], [Display "There is a limited amount of things you can do with a junk pile. Fortunately."])
    ,("cupboard", Examine, Nothing, [HasStatus Closed], [Display "It's a closed cupboard."])
    ,("cupboard", Examine, Nothing, [HasStatus Opened], [LookInsideContainer "cupboard" "This is your typical kitchen cupboard, containing : "])
    ,("cupboard", Open, Nothing, [], [ChangeStatus "cupboard" Opened, LookInsideContainer "cupboard" "You open the cupboard and look at its content : "])
    ,("cupboard", Search, Nothing, [], [RebranchTo Examine "cupboard" Nothing])
    ,("cupboard", Take, Nothing, [], [Display "It's heavy. You're weak. And anyway, it's a stupid idea. No offense."])
    ,("cupboard", Take, Just "salt", [], [PickFromContainer "cupboard" "salt", Display "You are now the proud owner of an excellent way of protecting yourself from fairies. Neat."])
    ,("cupboard", Take, Just "saucepan", [], [PickFromContainer "cupboard" "saucepan", Display "You take the saucepan. Yeah, that's really useful when going after a vampire."])
    ,("cat bowl", Use, Just "Zinc bit", [], [PutInsideContainer "cat bowl" "Zinc bit" "You've put the zinc in the bowl. Look at you, doing chemistry or whatever !"])
    ,("cat bowl", Use, Just "Copper bit", [], [PutInsideContainer "cat bowl" "Copper bit" "You've put the copper in the bowl. That'll teach him !"])
    ,("cat bowl", Use, Just "Electric wires", [ContainsAmountOfItem (== 2), HasStatus Salted], [PutInsideContainer "cat bowl" "Electric wires" "You plug the wires to the zinc and copper bits. You did it ! You know have a source of electricity that would make Tesla proud !"])
    ,("cat bowl", Use, Just "Electric wires", [], [Display "No, something is still missing, there."])
    ,("cat bowl", Use, Just "Salt", [], [ChangeStatus "cat bowl" Salted, Display "You put salt in the water. That's not very nice for the pet that'll try to drink it, you know."])
    ,("cat bowl", Take, Nothing, [], [Display "And deprive a cat, dog, or monstrous vampiric pet of his drink ? NOT ON MY WATCH !"])
    ,("cat bowl", Examine, Nothing, [ContainsAmountOfItem (== 0)], [Display "Just normal water in a bowl. Nothing to write home about."])
    ,("cat bowl", Examine, Nothing, [ContainsAmountOfItem (== 1)], [Display "A bowl with a bit of metal inside. How weird !"])
    ,("cat bowl", Examine, Nothing, [ContainsAmountOfItem (== 2)], [Display "A bowl with one bit of zinc and one bit of copper inside. This is weird indeed !"])
    ,("cat bowl", Examine, Nothing, [ContainsAmountOfItem (== 3)], [Display "A bowl with two bits of metal and electric wire connected to them... OK. OK."])
    ,("car bowl", Talk, Nothing, [], [Display "You complain to the bowl for a while about how hard your life is, what with your tendency to speak to inanimous objects and all."])
    ,("oven", Examine, Nothing, [], [Display "A wonderful old oven, as they used to do them in the old days. It works a bit like a fireplace. You could use it to cook anything. Even meth, I think. Don't do it, though. Meth is bad. I didn't just encourage you to do such a thing."])
    ,("oven", Use, Nothing, [], [Display "You need to put wood and start a fire"])
    ,("saucepan", Examine, Nothing, [], [Display "Well, it's a saucepan. It's been used before, but still ready to fry stuff if neeeded."])
    ,("salt", Examine, Nothing, [], [Display "It's a saltshaker. With salt inside. I WOULD give you the name of the brand, but they refused to sponsor this game, so you know. I won't."])
    ,("salt", Use, Nothing, [], [Display "Use it how ? To throw behind your shoulder ? OH COME ON, don't tell me you believe this ? OK, vampire, werewolf and zombies do exist in this game, but fairies ? Please."])
    ,("salt", Eat, Nothing, [], [Display "It wouldn't be good for your arteries. I'm doing you a favour."])
    ,("salt", Talk, Nothing, [], [Display "The salt is not answering. Ever considered having your mental health tested ? Just asking."])
    ,("brasero", Use, Just "mysterious paper", [HasStatus Luminescent], [Display "TODO !"])
    -- HERE, WE NEED TO MODIFY. Should be Maybe, Just x or Anything.
    ,("brasero", Use, Nothing, [HasStatus Luminescent], [Display "What do you want to use the brasero for ?"])
    ,("brasero", Use, Nothing, [ContainsAmountOfItem (== 1)], [Display "There is some coal in the brasero. If you have a lighter or a match, you can try to start a fire."])
    ,("brasero", Use, Nothing, [], [Display "The brasero is not lit. You need some kind of fuel to light it."])
    ,("brasero", Examine, Nothing, [HasStatus Luminescent], [Display "There is a nice fire in the brasero to warm you up."])
    ,("brasero", Examine, Nothing, [], [Display "The brasero is not lit. If you had any kind of fuel and something to start a fire, you could probably use it though."])
    ,("Sad", Examine, Nothing, [], [Display "Are we really going to have to spend time considering this emo character ? OK, obviously he is not a vampire. Most likely, he would love to be one. In any case, he's going to make you waste your time. And be annoying. Don't tell me I haven't warned you."])
    ,("Sad", Talk, Nothing, [], [Conversation necrotopics necroanswers undefined])
    ,("torchlight", Use, Just "battery", [], [ChangeStatus "torchlight" Powered, Display "Great, you now have a functioning torchlight. This should shed some light on your current issues. Shed some light... got it ? HAHAHAHA. Ahem. Sorry."])
    ,("torchlight", Use, Nothing, [HasStatus Powered], [ChangeStatus "torchlight" Luminescent, Display "You turn the torchlight on. You now have a lightsource and can go in dark places !"])
    ,("torchlight", Use, Nothing, [HasStatus Luminescent], [ChangeStatus "torchlight" Powered, Display "You turn the torchligts off. You have no lightsource now. Avoid dark places !"])
    ,("torchlight", Use, Nothing, [], [Display "There are no batteries in the torchlight !"])
    ,("torchlight", Examine, Nothing, [HasStatus Powered], [Display "A torchlight in almost perfect condition."])
    ,("torchlight", Examine, Nothing, [HasStatus Luminescent], [Display "The torchlight is on. Looking at it would blind you. You're not very bright, are you ? HAHAHAHAHA, GOT YOU AGAIN !"])
    ,("torchlight", Examine, Nothing, [], [Display "Well, it doesn't look like it's broken... but there is no battery inside. So, you know. It's not going to work. But it could. Well, maybe. Most likely, the rats ate the wire inside of this thing. I'm just saying, you know, don't be too hopeful."])
    ,("stanislas", Examine, Nothing, [], [Display "A portrait of Stanilas Lambdacula, count of the castle between 1357 and 1402. A fine looking fellow, actually."])
    ,("dolores", Examine, Nothing, [], [Display "A portrait of Dolores Lambdacula, countess of the castle. Born in 1369, She married Stanislas of Lambdacula in 1385 and ruled till her son was old enough, in 1408. She died in 1423. Wait a second... Dolores doesn't sound like a very Romanian name ! She must have come from Spain or something."])
    ,("Igor", Examine, Nothing, [], [Display "A portrait of Count Igor Lambdacula, son of Stanislas and Dolores, born in 1392. He ruled from 1408 to his death in 1459. Igor didn't look like a very fun guy, considering how dak and brooding he looks on this painting."])
    ,("Sveltana", Examine, Nothing, [], [Display "A portrait of Countess Sveltana Lambdacula, wife of Igor Lambdacula, born in 1397. She married him in 1412. Truth be told, she was quite beautiful. She died in 1453."])
    ,("unnamed portrait", Examine, Nothing, [HasStatus Nada], [Display "It's weird, there is no name below this portrait. Must be one of the Counts that ruled over the castle, but no informations about him. He's quite dashing. A bit pale, though."])
    ,("unnamed portrait", Examine, Nothing, [HasStatus Fixed], [Display "You dropped white spirit on the portrait, embracing your inner call towards vandalism. Since then, one can read on the bottom right side : \"Count Vlad Lambdacula, born in 1418.\"."])
    ,("unnamed portrait", Use, Just "white spirit", [PlayerHasObject "white spirit"], [Display "Oblivious of the fact you are most likely desacrating an historical treasure, you pour white spirit on the painting. You manage to scrap the bottom right corner of the canvas. There is something written here ! \"Count Vlad Lambdacula, born in 1418.\" Hmm... no date of death. Could it be... ?"])
    ,("unnamed portrait", Zilch, Nothing, [], [Display "I don't want to know how you got this idea. Not going to work."])
    -- TODO : Make sure aliases can be used to group reactions with synonyms.
    ,("portrait", Take, Nothing, [], [Display "You're not a burglar. Besides, you don't want to carry this thing with you all the time."])
    ,("portrait", Zilch, Nothing, [], [Display "You can't do much with this portrait."])
    ,("soda vending machine", Examine, Nothing, [], [Display "Well, it's a soda vending machine that looked like it was tuned into a cocktail vending machine. But in only serves bloody mary according to the labels on the buttons. Weird. Anyway, the machine is not plugged. And there doesn't even seem to be anything to plug it, so... forget your bloody mary."])
    ,("soda vending machine", Attack, Just "crowbar", [PlayerHasObject "crowbar"], [PickItem "coin", RemoveItem "crowbar", Display "Ha ! I like a thug with some method. Ok, you apply the crowbar on the machine. After a loud \"clang\", you manage to brreak the crowbar, so you won't be able to use it anymore. But there are good news : ONE coin fell on the ground. You pick it up quickly. You are now the proud owner of a coin !"])
    ,("soda vending machine", Attack, Nothing, [], [Display "There is nothing like gratuitous violence. However, as you try to kick the machine with your foot, you hurt yourself. If you want to continue this aggressive behaviour, you are most likely going to need help. Come back with a gang of baddies. Or with some kind of weapon."])
    ,("soda vending machine", Zilch, Nothing, [], [Display "I won't let you do stupid things to a vending machine. Tempting as it might be."])
    ,("dresser", Examine, Nothing, [HasStatus Closed], [Display "It is a massive, walnut-made dresser with glass paned doors. Though the glass is very, very dusty you can see through it that there is almost nothing to see inside. You'd probably could check a bit more if you opened it."])
    ,("dresser", Examine, Nothing, [HasStatus Opened, ContainsAmountOfItem (> 0)], [LookInsideContainer "dresser" "The dresser contains the following items : "])
    ,("dresser", Examine, Nothing, [HasStatus Opened, ContainsAmountOfItem (== 0)], [Display "The dresser is empty. You took everything. You greedy bastard."])
    ,("dresser", Open, Nothing, [], [ChangeStatus "dresser" Opened, LookInsideContainer "dresser" "You open the dresser, and in a cloud of dust, you manage to check the inside."])
    ,("dresser", Take, Just "fork", [], [PickFromContainer "dresser" "fork"])
    ,("dresser", Take, Just "emetic", [], [PickFromContainer "dresser" "emetic"])
    ,("dresser", Zilch, Nothing, [], [Display "The dresser was most likely not designed for this kind of use."])
    ,("emetic", Examine, Nothing, [], [Display "It's an emetic, namely something used to make you vomit."])
    ,("emetic", Eat, Nothing, [], [Display "No, you're not anorexic. And things are already nauseating enough."])
    ,("emetic", Zilch, Nothing, [], [Display "The less you interact with this stuff, the better you'll feel."])
    ,("fork", Examine, Nothing, [], [Display "It is a metallic fork. Really nothing special about it."])
    ,("fork", Eat, Nothing, [], [Display "No, no, no, you do not EAT FORKS. You USE THEM TO EAT. I know, it's a lot of stuff to understand. But I'm sure you'll manage."])
    ,("fork", Zilch, Nothing, [], [Display "One player. One fork. Not a whole lot of possibilities, there."])
    ,("mummy", Examine, Nothing, [], [Display "Well, this guy looks like a mummy in an armchair."])
    ,("mummy", Talk, Nothing, [], [Conversation mummytopics mummyanswers undefined])
    ,("mummy", Give, Just "Lady's Chatterley's Lover", [PlayerHasObject "book"], [GetFromCharacter "mummy" "dentures", Display "\"THANK GOD ! This sounds MUCH BETTER than what I'm currently reading. Not that it's a challenge. There, take my dentures, with that, you'll look like a real werewolf.\"", RemoveItem "Lady's Chatterley's Lover",Display "You are now the proud owner of a very ancient set of dentures."])
    ,("dentures", Eat, Nothing, [], [Display "No, no, no, YOU EAT WITH DENTURES. You do not EAT them. Is that so complex ?"])
    ,("dentures", Examine, Nothing, [], [Display "They are squeaky clean. Most likely, the mummy did not use them very often. Thank god."])
    ,("dentures", Use, Nothing, [], [Display "You put the denture in your mouth. It fits. If you had a mirror, you could see that you now have some wolfish quality."])
    ,("coin", Examine, Nothing, [], [Display "Some romanian coin. Let's face it : you didn't take time to study the local currencies. Me neither. So neither you nor me have the slightest idea how much it's worth. Most likely not a lot."])
    ,("coin", Zilch, Nothing, [], [Display "No, no, no, you don't want to anything that would make you risk this coin."])
    ,("plant", Examine, Nothing, [ContainsAmountOfItem (==1)], [Display "OK, this thing is disgusting. It has an enormous, bulky, green, sort of \"belly\", most likely used for digestion. Looking at its mouth. Around it, a bunch of dead insects. Refuse, or snack for later. You don't want to know."])
    ,("plant", Examine, Nothing, [ContainsAmountOfItem (==0)], [Display "It looks still a bit sick and dizzy, but you have no regrets."])
    ,("plant", Talk, Nothing, [], [Display "It is a PLANT. OK, it can eat stuff, but it doesn't mean it's able to talk."])
    ,("plant", Attack, Nothing, [], [Display "You don't stand a chance. This thing is HUGE. Trust me, I'm doing you a favour."])
    ,("plant", Give, Just "stew", [], [Display "You consider for a moment how sad your life is. You're now a cook for a carnivorous plant. Frankly, is it what you want to do ? No. But there might be an idea, there... You're just missing the main ingredient."])
    ,("plant", Give, Just "emetic stew", [], [Display "Good idea ! You give the revolting, insect stew to the vegetal monster. It slurps everything - even the pan ! After a few second, though, it begins belching. As you start to feel quite nauseous yourself, you see the plant... errr... give back a lot of things it ate. I'll spare you the details. In the middle, though !, you find a couple of electrical wires, that you take with you as a trophie."])
    ,("plant", Give, Just "saucepan", [], [Display "It is most likely hungry, but it won't eat just a pan with nothing inside."])
    ,("plant", Eat, Nothing, [], [Display "Yeah... you see, IT does the eating, generally, so you'd better thing something else to bite."])
    ,("liquor cabinet", Open, Nothing, [HasStatus Closed], [ChangeStatus "liquor cabinet" Opened, LookInsideContainer "liquor cabinet" "The cabinet only contains : "])
    ,("liquor cabinet", Take, Just "chalice", [], [PickFromContainer "liquor cabinet" "chalice"])
    ,("liquor cabinet", Examine, Nothing, [], [Display "A simple liquor cabinet made out of oakwood. Elegantly crafted, but it has seen better days."])
    ,("chalice", Drink, Nothing, [], [Display "You're not a vampire, you don't drink blood. And you don't know where this blood has been, anyway."])
    ,("chalice", Examine, Nothing, [], [Display "Well, it's a gold chalice with blood inside. What more do you need that screams \"Vampires are around !\" ?"])
    -- Crypt
    ,("statue", Examine, Nothing, [], [Display "You know nothing about sculpture, but this one is really ugly. But there is something interesting : there are marks on the floor underneath, as if the statue had be moved recently."])
    , ("statue", Push, Nothing, [], [Display "OK, this thing is heavy. Like in, way too heavy for you to move it. The best thing there would be to ask Arnold Schwarzenegger for help. But California is far away, and you don't really know him personnaly, so that would be a bit weird."])
    ]

-- Antichamber
-- OBJECTS
simpleObject :: [String]                -- Names 
                -> String               -- Room
                -> String               -- Description
                -> RoomObject           -- A proper room object
simpleObject a b c = objectContaining a b c Nada []

objectContaining :: [String]           -- Names
                -> String               -- Room
                -> String               -- Description
                -> ObjectStatus         -- Status of the object
                -> [RoomObject]         -- Content
                -> RoomObject           -- A proper room object
objectContaining aliases room description status contained = RoomObject naming room details
    where
        naming = ObjectNames aliases
        details = RoomObjectDetails status description contained

makeExit :: [String]     -- Aliases
            -> String       -- Room where this exit should be
            -> String       -- Description of the object
            -> String       -- Destination
            -> RoomObject   -- An Exit
makeExit aliases inRoom description destination = exitBuilder aliases inRoom description destination Opened

makeHiddenExit :: [String]  -- Aliases
            -> String       -- Room where this exit should be
            -> String       -- Description of the exit
            -> String       -- Exit destination
            -> RoomObject   -- An exit
makeHiddenExit aliases inRoom description destination = exitBuilder aliases inRoom description destination Hidden

exitBuilder :: [String] -> String -> String -> String -> ObjectStatus -> RoomObject
exitBuilder aliases inRoom description destination status = Exit (ObjectNames aliases) inRoom (RoomObjectDetails status description []) Nothing destination


makeDoor :: [String]     -- Aliases
            -> String       -- Room where this exit should be
            -> String       -- Description of the object
            -> String       -- Destination
            -> Maybe String -- Key ?
            -> ObjectStatus -- Opened / Closed / Hidden ?
            -> RoomObject   -- An Exit
makeDoor alias inRoom description room key status = Exit (ObjectNames alias) inRoom (RoomObjectDetails status description []) (Just (DoorInfo key)) room

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
        , Room "The countess bathroom" "It shall not be said that the transylvanian countryside is devoid of the comfort and sophistication of the modern life. This is a fully equipped bathroom, with an elegant porcelain bathtub. You can still smell a feminine fragrance in the air, but maybe it's just your imagination." Nada
        , Room "The private living room" "This little cosy room was used by the family of the former Counts of the castle when they didn't entertain guests. There are only two armchairs and a two-sitter. Someone with bad decorative taste decided that a yellow wallpaper would brighten the room. It doesn't." Nada 
        , Room "The southern corridor" "You are walking in a cold corridor. Openings in the wall, without glasspane, help freshen the temperature in this place. A massive staircase goes down to the main hall of the castle. On your left and on your right, two opened doors. Apart from that and an insane amount of cobwebs, the place is empty." Nada
        , Room "The laboratory" "This is your classical Dr. Frankenstein styled laboratory. Iron tables covered with sciency stuff that you would be able to recognize had you paid attention in chemistry classes. Retorts, still, tubes, the whole thing, even the operating table with straps and blood stain." Nada
        , Room "The observatory" "What did you know ? Count Lambdacula must be interested in astronomy. He set up a nice observatory here, with a big telescope and stars-chart along the walls. Even vampires look at the sky at night, and make wishes while watching shooting stars. Well, their wishes are mostly things like fountains of blood." Nada
        , Room "The Count's sauna" "Count Lambdacula was not having a bathroom, no, that's for normal people, not vampire psychopaths. The guy had himself a whole sauna set-up. And looking at the quality of the place, he must have asked some crazy scandinavian specialists. You're standing in a state-of-the-art sweating lodge, made out of the purest wood. The only thing is that you're a bit hot right now." Nada
        -- UNDERGROUND WORLD
        , Room "A dark corridor" "You're walking on a creepy natural corridor. Far-away sounds, echoing through the walls, give you the creeps. You know, there is a ladder right behind you, leading to a hatch, that will allow you to leave this underground madness. I'm just saying. Nobody will be judging you if you act like a coward. I mean not everyone is cut out to be a hero, right ? Let's face it, you should be working in a cubicle, right now. Not dwelve in the heart of a Transylvanian mountain, where some monsters will most likely tear your chest apart and make a supper out of your brain.\nAnyway, the corridor continues to the south. In the darkness. With lots of creepy sounds. Not to scare you or anything." Dark
        -- End rooms
        , Room "The crypt" "You've entered a dark, dark crypt where the Lambdacula family members are supposed to be buried. But the place has been completly emptied. There is dust and ash on the ground. But you notice a little door on the northern wall." Dark]

ldObjects = [makeExit ["South"] "In front of the castle" "the gate of the castle" "The southern gate" 
            ,makeExit ["East"] "In front of the castle" "a path on the mountain" "A muddy path" 
            -- Southern gate
            ,makeExit ["North"] "The southern gate" "the gate of the castle" "In front of the castle"
            ,makeExit ["South"] "The southern gate" "the rest of the yard" "The castle entrance"
            ,makeDoor ["West", "metallic door", "metal door"] "The southern gate" "A metallic door" "The guard room" Nothing Closed
            ,makeDoor ["East", "wooden door", "wooden door"] "The southern gate" "A wooden door" "The kitchen" Nothing Closed
            ,objectContaining ["pile", "pile of junk", "junk", "refuse"] "The southern gate" "There is a pile of junk next to the gates of the castle, and if you stay there too long, I'd say a pool of vomit close to it." Opened [simpleObject ["a torchlight", "torchlight", "torch"] "NOWHERE" "A torchlight"]
            -- Castle entrance
            ,makeExit ["North"] "The castle entrance" "To the southern gate" "The southern gate"
            ,makeDoor ["South", "double doors", "doors", "door"] "The castle entrance" "An impressive double doors" "The hall" Nothing Closed
            -- Kitchen
            ,makeDoor ["West", "wooden door", "wooden door"] "The kitchen" "A wooden door" "The southern gate" Nothing Closed
            ,makeExit ["South"] "The kitchen" "To an antichamber" "Antichamber"
            ,objectContaining ["cupboard", "cupboards"] "The kitchen" "You notice one cupboard that seem in better state than the others." Closed 
                [simpleObject ["saucepan", "pan"] "" "A saucepan"
                ,simpleObject ["salt", "saltcellar", "salt cellar", "saltshaker"] "" "A saltshaker"
                ]
            ,simpleObject ["cat bowl", "bowl"] "The kitchen" "On the floor, there seem to be a bowl with some water in it, for a cat. Or a dog. Whatever it is, it's not around anyway."
            ,simpleObject ["oven", "old oven"] "The kitchen" "The old oven is still there, a tribute to the quality of the craftsmanship of yore."
            -- Guard room
            ,makeDoor ["East", "metallic door", "metal door"] "The guard room" "A metallic door" "The southern gate" Nothing Closed
            ,makeExit ["West", "wooden door", "wooden door"] "The guard room" "An opened door" "The dormitory"
            ,makeExit ["South"] "The guard room" "An impressive corridor" "A gallery" 
            ,makeExit ["Up", "upstairs"] "The guard room" "Stairs leading to the second floor" "The north-west corridor"
            ,simpleObject ["brasero"] "The guard room" "One of the braseros seems in perfect condition."
            -- Dormitory
            ,makeExit ["East", "wooden door", "wooden door"] "The dormitory" "An opened door" "The guard room" 
            ,simpleObject ["Sad", "man", "goth", "necromancer", "necro", "man in robes", "man in black robes"] "The dormitory" "there is a man in black robes, wearing make up to look as pale as possible. He's got skull-shaped rings on almost all his fingers."
            -- Gallery
            ,makeExit ["North"] "A gallery" "To the guard room" "The guard room"
            ,makeExit ["South"] "A gallery" "To the living room" "The living room"
            ,simpleObject ["Stanislas","portrait of Stanislas of Lambdacula", "portrait", "Stanislas of Lambdacula"] "A gallery" "A portrait of Stanislas of Lambdacula !"
            ,simpleObject ["Dolores", "portrait of Dolores of Lambdacula", "portrait", "Dolores of Lambdacula"] "A gallery" "A portrait of Dolores of Lambdacula !"
            ,simpleObject ["Igor", "portrait of Igor of Lambdacula", "portrait", "Igor of Lambdacula"] "A gallery" "A portrait of Igor of Lambdacula !"
            ,simpleObject ["Sveltana", "portrait of Sveltana of Lambdacula", "portrait", "Sveltana of Lambdacula"] "A gallery" "A portrait of Sveltana of Lambdacula !"
            ,simpleObject ["unnamed portrait", "portrait of a man without a name", "portrait", "anonymous portrait", "portrait without a name", "nameless man portrait", "portrait of a nameless man"] "A gallery" "A portrait of a man without a name !"
            -- Antichamber
            , makeExit ["North"] "Antichamber" "To the kitchen" "The kitchen"
            , makeExit ["East"] "Antichamber" "To the dining room" "The dining room"
            , makeExit ["South"] "Antichamber" "To the smoking room" "The smoking room"
            , objectContaining ["soda vending machine", "vending machine", "machine"] "Antichamber" "Against one wall, there is a slightly incongruous vending machine." Nada [simpleObject ["coin", "romanian coin"] "" "A coin."]
            -- Dining room
            , makeExit ["West"] "Dining room" "To the antichamber" "The antichamber"
            , objectContaining ["dresser"] "Dining room" "On the wall opposite to the door, there is a massive dresser to store crockery." Opened
            [simpleObject ["emetic", "A vial of emetic", "drugs", "vial of emetic", "vial"] "" "A vial that, according to its label, contains a potent emetic. Don't ask me what it is doing there."
            ,simpleObject ["fork"] "" "A simple fork. That's all there's left of all the cutelry and crockery that must have been stored here once."]
            -- Living room
            , makeExit ["North"] "The living room" "To the gallery" "A gallery"
            , makeExit ["West"] "The living room" "To the library" "The library"
            , makeExit ["East"] "The living room" "To the hall" "The hall"
            , objectContaining ["liquor cabinet", "cabinet"] "The living room" "There is a little liquor cabinet in the corner of the room, where you'd normally store your best whisky, crystal glasses and the like." Closed [simpleObject ["chalice", "blood"] "" "The only thing here is a golden chalice full of fresh blood. Yikes."]
            -- The library
            , makeExit ["East"] "The library" "To the living room" "The living room"
            -- The hall
            , makeExit ["West"] "The hall" "To the living room" "The living room"
            , makeDoor ["North", "double doors", "doors", "door"] "The hall" "An impressive double doors" "The castle entrance" Nothing Closed
            , makeExit ["East"] "The hall" "To the smoking room" "The smoking room"
            , makeExit ["South"] "The hall" "To a chapel" "The chapel"
            , makeExit ["Up", "upstairs"] "The hall" "To the first floor, or, if you're american, the second floor" "The southern corridor"
            -- The chapel
            , makeExit ["North"] "The chapel" "To the main hall" "The hall"
            , makeHiddenExit ["Down", "downstairs", "downward"] "The chapel" "A dark staircase leading to a mysterious crypt, ooh, scary !" "The crypt" 
            , simpleObject ["statue", "ugly statue", "angel"] "The chapel" "On the side, there is a particularly ugly statue of an angel."
            -- The smoking room
            , makeExit ["East"] "The smoking room" "To an inner garden" "The inner garden"
            , makeExit ["North"] "The smoking room" "To an antichamber" "Antichamber"
            , makeExit ["West"] "The smoking room" "To the hall" "The hall"
            , objectContaining ["mummy", "akhoris", "man in bandages", "man"] "The smoking room" "Sitting in one the armchair, reading a book, there is a man covered by bandages. Weird !" Nada [simpleObject ["dentures", "a wolfish dentures"] "" "A set of wolfish dentures"]
            -- Inner garden
            , makeExit ["West"] "The inner garden" "To the smoking room" "The smoking room"
            , makeExit ["South"] "The inner garden" "To the conservatory" "The conversatory"
            , objectContaining ["plant", "carnivorous plant"] "The inner garden" "In the middle of the garden, an enormous vegetal specimen with impressive thorn, that certainly looks like a carnivorous plant." Opened [simpleObject ["electric wires", "wires"] "" "A couple of electric wires with little pliers, the kind of stuff you'd see in a school."]
            -- The northwest corridor
            , makeExit ["Down", "downstairs"] "The north-west corridor" "Stairs to the first floor" "The guard room"
            , makeDoor ["West", "wooden door", "door"] "The north-west corridor" "A wooden door" "The countess bedroom" Nothing Closed
            , makeExit ["East"] "The north-west corridor" "To the north-east corridor" "The north-east corridor"
            -- The countess bedroom
            , makeDoor ["East", "wooden door"] "The countess bedroom" "A wooden door" "The north-west corridor" (Just "silver key") Closed
            , makeExit ["South"] "The countess bedroom" "To the bathroom" "The countess bathroom"
            -- The northeast corridor
            , makeExit ["West"] "The north-east corridor" "To the north-west corridor" "The north-west corridor"
            , makeDoor ["East", "iron door", "door"] "The north-east corridor" "An iron door, with mysterious, beeping sounds behind" "The amusement arcade" (Just "skull key") Closed
            , makeExit ["South"] "The north-east corridor" "To the central corridor" "The central corridor"
            -- The amusement arcade
            , makeExit ["West"] "The amusement arcade" "To the corridor" "The north-east corridor"
            , simpleObject ["Roger", "engineer", "dude", "man"] "The amusement arcade" "Behind a counter, there is a slightly overweight beardy guy in blue overalls, rumaging through a toolbox."
            , simpleObject ["Video game", "Galambdaga", "video arcade"] "The amusement arcade" "The only game working is a 1992 antique version of Galambdaga, the very classic space shooter !"
            -- The central corridor
            , makeExit ["North"] "The central corridor" "The corridor continues to the north" "The north-east corridor"
            , makeExit ["South"] "The central corridor" "The corridor continues to the south" "The southern corridor"
            , makeDoor ["West", "leathered door", "door"] "The central corridor" "A door covered with leather" "The count bedroom" (Just "black key") Closed
            , makeExit ["East"] "The central corridor" "A passage towards what looks like a living room" "The private living room"
            -- The count's bedroom
            , makeExit ["East"] "The count bedroom" "Back to the central corridor" "The central corridor"
            -- The private living room
            , makeExit ["West"] "The private living room" "Back to the central corridor" "The central corridor"
            -- The southern corridor
            , makeExit ["North"] "The southern corridor" "To the central corridor" "The central corridor"
            , makeExit ["Down", "downstairs"] "The southern corridor" "A stairway to the main hall" "The hall"
            , makeDoor ["East", "door", "wooden door"] "The southern corridor" "A small wooden door" "The Count's sauna" Nothing Closed
            , makeExit ["West"] "The southern corridor" "An archway to a laboratory" "The laboratory"
            -- The laboratory
            , makeExit ["East"] "The laboratory" "Back to the southern corridor" "The southern corridor"
            , makeExit ["West"] "The laboratory" "A little arch leading to a round room" "The observatory"
            -- The observatory
            , makeExit ["East"] "The observatory" "A little arch leading back to the laboratory" "The laboratory"
            -- The sauna
            , makeExit ["West"] "The Count's sauna" "Back to the southern corridor" "The southern corridor"
            -- Muddy path
            ,makeExit ["West"] "A muddy path" "an upward path to the castle" "In front of the castle" 
            ,makeDoor ["A door", "door"] "A muddy path" "the gamekeeper shack" "The gamekeeper shack" Nothing Closed 
            -- Gamekeeper shack
            ,makeDoor ["A door", "door"] "The gamekeeper shack" "a way out to the path below Lambdacula's Castle" "A muddy path" Nothing Closed 
            ,simpleObject ["Lady's Chatterley's Lover", "book", "Lady Chatterley"] "The gamekeeper shack" "On the night table, an old copy of Lady Chatterley's Lover."
            ,simpleObject ["the rug", "rug", "oriental touch"] "The gamekeeper shack" "Decorum is not really the place forte, though there is a nice rug on the floor."
            ,makeDoor ["A hatch", "the hatch", "hatch"] "The gamekeeper shack" "Downwards to the unknown" "A dark corridor" Nothing Hidden]

-- cONVERSATIONS
necrotopics = [("hello", ["hi"]), ("Sad", ["sadness", "being sad", "him being sad"]), ("mother", ["mum", "mummy"]), ("happy", []), ("necromancer", ["necro", "necromancy"]), ("dissertation", ["essay", "topic"]), ("death", []), ("taxes", ["tax"]), ("count", ["count lambdacula"]),("crypt", ["secret crypt"]), ("vampire", ["vampiric society", "vampires", "vampire society"]), ("transylvania", []), ("zombies", ["zombie"])]
necroanswers =  [("hello", "Hellooooo dude. I'm sad.")
    ,("Sad", "No, no, I'm pretty happy. My NAME is Sad is all. You know. My mother was kinda depressed when she had me. It was supposed to be just an ordinary case of baby blues, but actually, it never stopped. I think she's better now I've left her to become a Necromancer.")
    ,("mother", "My mother was a tailor, my father was a gambler... you know how it goes.")
    ,("happy", "Look, man, this is Lambdacula Castle. There is no BETTER PLACE to be for a Necromancer.")
    ,("necromancer", "Yes, that's my major. I wanted to major in arts and stuff, but in this economy... Anyway, it turned out to be a passion, so I'm doing my PhD in Necromancy. I'm doing this trip here to collect data for my dissertation.")
    ,("dissertation", "What's my dissertation about ? \"Adoration of Death and Cult of Decline amongst Transylvanian Vampiric Communities from a Postmodern Point Of View\". I know, the title is too short, it doesn't look serious...")
    ,("death",  "Look, dude, I spend my WHOLE DAY talking, reading and writing about death, I'd like to speak about something else. Like taxes, for instance. Taxes are fun.")
    ,("taxes", "Did you know, for instance, that Count Lambdacula is a very law-abiding citizen ? He pays his taxes every year. Well, that's what I've heard.")
    ,("count", "The count ? I've never met him. I hope to. The rumour is he's sleeping in a secret crypt somewhere in the castle.")
    ,("crypt", "Well it's been fashionable to have a secret crypt in the vampire society for quite a long time, you know. I have no idea where it is though.")
    ,("vampire", "Vampires are living dead who drink blood. I think that you should know that before coming to Transylvania, you know.")
    ,("transylvania", "Well, it's a nice place, you know. Particularly if you like vampires. Which I do. I mean, come on, I know some people specialize on zombies, but, seriously, ZOMBIES ? Walking rotting corpses with no brain whatsoever ? That's sick, man.")
    ,("zombies", "I think they are some around, but frankly, my dear, I don't give a damn.")
    ,(notopic, "I have nothing to say about that.")] 

mummytopics = [(helloTopic,["hi"]), ("vlad", []), ("akhoris", []), ("vampire", ["count", "undead"]), ("protection", ["protect myself", "protect"]), ("werewolf", ["werewolves"]), ("fake hair", ["hair"]), ("fake teeth", ["teeth"]), ("t-shirt", ["greenpeace"]), ("dentures", []), ("book", []), ("retirement", []), ("pharaoh", []), ("amnesia", [])] 

mummyanswers = [("hello", "Well hello ! Delighted to meet you. My name is Akhoris, first scribe of the Great Pharaoh and... oh, I'm afraid that's ancient history. I'm enjoying my retirement, you see.")
        ,("vlad", "Vlad is the Count of this castle. A very nice fellow, provided you are undead. You being of the still-living disposition, I'm afraid he could consider you more like a meal than like an opportunity for conversation.") 
        ,("akhoris", "That is my name. I have no idea what it means. You see, I've lost all my egyptian. I can't even draw anything properly. My good friend Vlad thinks I have got a severe case of amnesia.")
        ,("vampire", "Well, I don't want to sound biased... but let's just say vampires are much less pleasant than mummies. You know, we're often presented as malevolent and scary creatures but the truth is we're really nice guys. Whereas my good friend the Count has needs, like plunging his teeth in your throat. Of course, there are ways to protect yourself against such behaviour.")
        ,("protection", "Well there are the obvious ones : garlic, crosses and so on. But I wouldn't put too much faith in them. Sure, vampires don't like garlic and avoid crosses, but not to the point they'd refrain from slurping down your blood. No, the absolute anti-vampire protection is to let them think you are a werewolf.") 
        ,("werewolf", "Yes, you see vampires and werewolves are enemies. And werewolves are much deadlier. So a vampire will avoid any one-on-one confrontation. Now, of course, looking like a werewolf can be tricky. In their human form, werewolves tend to be hairy, have enormous teeth and be tree-huggers. So, I'd say you're going to need fake hair, fake teeth and something like a Greenpeace t-shirt or whatever.")
        ,("fake hair", "I have no idea where you'd find this, I'm sorry !")
        ,("fake teeth", "Not only fake teeth, but fake big, scary, wolfish teeth. Well, you are in luck, because it just so happen my dentures are exactly like that. I could give them to you.")
        ,("t-shirt", "You know, something that makes you look like you care about the planet. Ask Al Gore.")
        ,("dentures", "I COULD give them to you. But you know, I'm a buinessman, everybody must give something for everything they get. I won't give them out of pure charity.")
        ,("book", "Oh it's something I bought in the station coming here. It's a terrible book about a teenage girl and a vampire and werewolves and frankly, if you don't mind my saying, they all need to get laid. I'd kill to have a nice, steamy romance instead of this.")
        ,("amnesia", "I'm sorry, I don't remember speaking about this.")
        ,("pharaoh", "Oh, pompous old farts, the lot of them. I'm much better off here. Vlad has been so nice to invite me !")
        ,("retirement", "Well, you could also call it the afterlife. You see, my internal organs were removed from my body, I was covered in magical fluids and carefully bandaged. Which mean I can live forever without the... special needs of my good friend Vlad. So, now, I travel, I meet people, I read... I'm having the time of my afterlife.")
        ,(notopic, "I don't think I have interesting things to say about that... sorry.")]

techtopics = [(helloTopic, ["hi"]), ("Roger", ["roger the engineer"]), ("engineer", ["engineering", "job","work"]),("count", ["Vlad", "Count Lambdacula", "Lambdacula", "boss"]), ("vampire", ["pale", "pale guy", "healthy dentition"]), ("arcade", ["gaming", "games"]), ("Galambdaga", []), ("battery", ["dead battery"]), ("electricity", ["electrical"])]
techanswers = [("hello", "Listen kid, don't you see I'm busy ? Go play wi... oh wait, you're not a kid. Hi. My name is Roger. I'm the engineer.")
        ,("Roger", "Yes, that is my name. Roger the engineer. Gotta love the Yardbirds.")
        ,("engineer", "Well, engineer is quite a big word for what I do, I must admit. The thing is, a few years ago, I had this great job offer from the dude that owns the place... you know, Count Lambdacula ? Pale guy, quite the healthy dentition, never around during the day ? He wanted someone to set up and manage an arcade gaming room. So... here I am. Doing a terrible job.")
        ,("arcade", "I know, it is in a sorry state... but that's not really my fault. The electricity here is a disaster, let me tell you. And most of the thing my boss bought was in a terrible state already. I hardly have any tools... the Count keeps saying he'll buy me the stuff, but he never does. I could go to the town and buy stuff, but I prefer staying here, trying to fix the wiring system first.")
        ,("count", "The Count is my boss. He's almost never here. He comes in the evening sometimes. He plays mostly Galambdaga. I must say he's quite good. Well, he's a good chap. Pays me quite generously. Where is he right now ? No idea. Look man, I only an employee, here...")
        ,("vampire", "Vampire ? Don't be ridiculous, man, those things don't exist. You've read to much Twilight.")
        ,("Galambdaga", "Ah, that's a beauty, isn't it ? Must be quite expensive I think. Which is why whatever happens, I make sure this one works ! You can play, if you got a coin. I think any kind of coin works, really, the thingamajig that checks for the proper change is almost broken.")
        ,("battery", "Dead batteries ? Can't help you, friend. The only thing you could do is try to recharge them. But I have nothing to do this. Well, theoretically, if you connect electric wires to the battery to zinc and copper in salt water, it COULD work, but most likely, you'll destroy the batteries and get electrocuted. I would advise against it. I'm sure you could find copper or zinc somewhere in the castle, though.")
        ,("electricity", "Yeah, that's my biggest concern right now. If you need help with anything on this matter, I can perhaps give you a hand, but I'm quite busy !")]
