module Lambdacula (
    proceed
)
where
import Data.Char
import Control.Monad.State
import Control.Applicative
import qualified Data.Map as Map
import System.IO
import Action
import Parser
import World
import GameData

main = do
        printStrs . displayRoom $ currentRoom aWorld
        promptLoop aWorld

-- Display a prompt, get some input, call some proceeding function
-- to do stuff with it.
promptLoop :: World -> IO()
promptLoop world = do
    putStr "> "
    hFlush stdout 
    input <- getLine
    let action = processInput input verbs 
    if (isQuit action) 
        then putStrLn "K Thx Bye"
        else
            do
                let reaction = runState (proceed action) world 
                printStrs $ fst reaction
                promptLoop $ snd reaction 
    return ()

-- Check if this is a quit action.
isQuit :: PlayerAction -> Bool
isQuit (SimpleAction QuitGame) = True
isQuit _ = False

-- Given the world and an action, do some stuff... and analyze the world.
proceed :: PlayerAction -> State World [String]
proceed (SimpleAction Zilch) = singleAnswer "Huh ?"
proceed (SimpleAction Examine) = state $ \w -> (displayRoom $ currentRoom w, w)
proceed (Interaction act obj) = do 
                                w <- get
                                case findObjectInteraction obj (currentRoom w) of
                                    Nothing -> singleAnswer $ "There is no " ++ obj ++ " here !"
                                    Just func -> func act
proceed _ = singleAnswer "Whaaaat ?"

printStrs = do
            mapM putStrLn

promptOnAction :: RoomObject -> Action -> String
promptOnAction = getTextForAction

-- Display a room description to the player.
displayRoom :: Room -> [String] 
displayRoom (Room name desc _ _ exits) = 
                                [stars] ++ [map toUpper name] ++ [stars] ++ [desc] ++ displayExits
    where 
        stars = replicate (length name) '*' 
        displayExits = ["Exits : "] ++ ["\t" ++ x ++ "\n"|x <- map(show) exits] 

aWorld = World (Player []) room (mapFromRooms [room, room'])
