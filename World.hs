module World
(
    Room(..),
    RoomObject(..),
    Character(..)
)
where

import qualified Data.Map as Map
import Action

type Conversations = Map.Map String String
type Interactions = Map.Map Action String

testCube = RoomObject "Test cube" (Map.fromList[(Examine, "A simple test cube. Look, how pretty !"),
                                            (Talk, "You can't talk to a cube, don't be silly"),
                                            (Move, "You push the cube. Happy now ?")]) 

room = Room "The test room" "You are standing in a non-existant place in a virtual world. This is a very good place to hold existential thoughts. Or test the system. But this is more or less the same thing, innit ?" [testCube] [] [] 

data Room =    Room { roomName :: String
                 ,description :: String
                 ,objects :: [RoomObject]
                 ,characters :: [Character]
                 ,exit :: [Exit]
                }

data RoomObject = RoomObject { objectName :: String, objectReactions :: Interactions }
data Exit = Exit { exitDescription :: String, destination :: String }
data Character = Character {name :: String, aliases :: [String], topics :: Conversations, npcReactions :: Interactions }
