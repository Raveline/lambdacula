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

-- Display a room description to the player.
displayRoom :: Room -> [String] 
displayRoom (Room name desc _ _ exits) = 
                                [stars] ++ [map toUpper name] ++ [stars] ++ [desc] ++ displayExits
    where 
        stars = map (const '*') name 
        displayExits = "Exits : " : ["\t" ++ x ++ "\n"|x <- map show exits] 

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
displayContainerContent (RoomObject _ _ _ contained _) = [_objectName x| x <- contained]
