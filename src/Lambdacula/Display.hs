-----------------------------------------
-- Display.hs
-- Handle human-readable interactions.
-----------------------------------------
module Lambdacula.Display (
    displayRoom,
    format80,
    displayCurrentRoom,
    displayInventory,
    printStrs,
    singleAnswer,
    present
)
where

import Data.List.Split
import Data.List
import Data.Char
import Control.Lens 
import Control.Monad.State
import Control.Applicative

import Lambdacula.World
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
displayRoom (Room name desc status) ros
    | status /= Dark = [stars] ++ [map toUpper name] ++ [stars] ++ [desc ++ (displayObjects ros)] ++ ["Exits :"] ++ displayExits ros 
    | otherwise = displayDarkRoom
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
        -- BEWARE. Exits who are hidden should not be displayed.
        displayExits :: [RoomObject] -> [String]
        displayExits [] = []
        displayExits ((Exit nms _ _ desc _):ros)
            | _status desc /= Hidden = (("\t" ++ (headName nms) ++ " : " ++ (_objectDescription desc)):(displayExits ros))
            | otherwise = displayExits ros
        displayExits (_:ros) = displayExits ros
displayDarkRoom :: [String]
displayDarkRoom = ["It is pitch black. You are likely to be eaten by... no I'm kidding. You're just going to get hurt. Type \"flee\" to get back to safety."]
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


-- Display the player inventory
displayInventory :: [String] -> [String]
displayInventory [] = ["You have nothing, but clothes on your back. I won't comment on your taste, by the way."]
displayInventory xs = "You're currently the proud owner of the following items : ":(map ((++) "- ") xs)

printStrs = mapM putStrLn . format80


-- Used when State does not need to be changed.
-- Given a string, will return the World "as it is" and the string.
singleAnswer :: String -> WorldAction 
singleAnswer = return . (:[])

articles = ["A", "The"]
-- Display nicely a name with a cap first letter
-- Take off the article in front if needed.
present :: RoomObject -> String
present = capFirstLetter . (removeArticles . words) . mainName
    where
        capFirstLetter :: String -> String
        capFirstLetter [] = []
        capFirstLetter (x:xs) = toUpper x : xs
        removeArticles :: [String] -> String
        removeArticles [] = []
        removeArticles [x] = x
        removeArticles (art:wd) = unwords wd
