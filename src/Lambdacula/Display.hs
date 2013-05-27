-----------------------------------------
-- Display.hs
-- Handle human-readable interactions.
-----------------------------------------
module Lambdacula.Display (
    displayRoom,
    format80,
    displayContainerContent,
    displayCurrentRoom,
    displayInventory
)
where

import Lambdacula.World
import Data.List.Split
import Data.List
import Data.Char
import Control.Lens 
import Control.Monad.State

-- Given a state, will display the current room
displayCurrentRoom :: WorldAction
displayCurrentRoom = do 
                        room <- use currentRoom
                        objs <- use currentObjects
                        return $ displayRoom room objs

-- Display a room description to the player.
displayRoom :: Room         -- The room
            -> [RoomObject] -- The objects in this room
            -> [String]     -- The text to display
displayRoom (Room name desc) ros = 
                                [stars] ++ [map toUpper name] ++ [stars] ++ [desc ++ (displayObjects ros)] ++ ["Exits :"] ++ displayExits ros 
    where 
        -- Display a line of stars
        stars = map (const '*') name 
        -- Display a sentence describing each object in the continuity of the text
        displayObjects :: [RoomObject] -> String
        displayObjects = foldr ((++) . displayObjects') ""
        displayObjects' :: RoomObject -> String
        displayObjects' (RoomObject _ _ _ details) = " " ++ _objectDescription details
        displayObjects' _ = ""
        -- Display the exits on separate lines
        displayExits :: [RoomObject] -> [String]
        displayExits [] = []
        displayExits ((Exit nms _ _ desc _):ros) = (("\t" ++ (headName nms) ++ " : " ++ (_objectDescription desc)):(displayExits ros))
        displayExits (_:ros) = displayExits ros

-- Take a bunch of strings.
-- Format them so that they won't take more than 80 characters.
format80 :: [String] -> [String]
format80 strings = splitOn "\n" . format80' 0 $ (splitOn " ". intercalate "\n" $ strings)
    where
        format80' :: Int -> [String] -> String
        format80' _ [] = []
        format80' n (s:ss) 
            | "\n" `isInfixOf` s = s ++ " " ++ (format80' 0 ss)
            | length s + n >= 80 = "\n" ++ s ++ " " ++ (format80' (length s + 1) ss)
            | otherwise = s ++ " " ++ (format80' (n + length s + 1) ss)


-- Display the objects contained by a container
displayContainerContent :: RoomObject -> [String] 
displayContainerContent ro = [mainName x| x <- (ro^.containedObjects)]

-- Display the player inventory
displayInventory :: [String] -> [String]
displayInventory [] = ["You have nothing, but clothes on your back. I won't comment on your taste, by the way."]
displayInventory xs = "You're currently the proud owner of the following items : ":(map ((++) "- ") xs)

printStrs = mapM putStrLn . format80
