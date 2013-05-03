module Lambdacula.Display (
    displayRoom,
    format80,
    displayContainerContent
)
where

import Lambdacula.World
import Data.List.Split
import Data.List
import Data.Char
import Control.Lens 

-- Display a room description to the player.
displayRoom :: Room -> [String] 
displayRoom (Room name desc objs) = 
                                [stars] ++ [map toUpper name] ++ [stars] ++ [desc ++ (displayObjects objs)] ++ ["Exits :"] ++ displayExits objs
    where 
        -- Display a line of stars
        stars = map (const '*') name 
        -- Display a sentence describing each object in the continuity of the text
        displayObjects :: [RoomObject] -> String
        displayObjects = foldr ((++) . displayObjects') ""
        displayObjects' :: RoomObject -> String
        displayObjects' (RoomObject _ _ details) = " " ++ _objectDescription details
        displayObjects' _ = ""
        -- Display the exits on separate lines
        displayExits :: [RoomObject] -> [String]
        displayExits [] = []
        displayExits ((Exit nms _ desc):ros) = (("\t" ++ (headName nms) ++ " : " ++ (_objectDescription desc)):(displayExits ros))
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
