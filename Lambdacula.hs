module Lambdacula
( proceed
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
promptLoop :: World -> IO ()
promptLoop world = do
    input <- getAction
    let action = processInput input verbs
    either putStrLn (runAction world) $ quitOrContinue action

-- Execute an action
runAction :: World -> State World [String] -> IO ()
runAction world s = do
  printStrs $ fst reaction
  promptLoop $ snd reaction 
  where reaction = runState s world 

-- Ask for an action.
getAction :: IO String
getAction = do
  putStr "> "
  hFlush stdout
  getLine

-- Check if the proposed action is to quit or to do something.
quitOrContinue :: PlayerAction -> Either String (State World [String])
quitOrContinue (SimpleAction QuitGame) = Left "K thx Bye"
quitOrContinue a = Right $ proceed a

-- Given the world and an action, do some stuff... and analyze the world.
proceed :: PlayerAction -> State World [String]
proceed (SimpleAction Zilch) = singleAnswer "Huh ?"
proceed (SimpleAction Examine) = state $ (,) <$> displayRoom . currentRoom <*> id
proceed (Interaction act obj) = do 
                                w <- get
                                case findObjectInteraction obj (currentRoom w) of
                                    Nothing -> singleAnswer $ "There is no " ++ obj ++ " here !"
                                    Just func -> func act
proceed _ = singleAnswer "Whaaaat ?"

printStrs = mapM putStrLn

promptOnAction :: RoomObject -> Action -> String
promptOnAction = getTextForAction

-- Display a room description to the player.
displayRoom :: Room -> [String] 
displayRoom (Room name desc _ _ exits) = 
                                [stars] ++ [map toUpper name] ++ [stars] ++ [desc] ++ displayExits
    where 
        stars = map (const '*') name 
        displayExits = "Exits : " : ["\t" ++ x ++ "\n"|x <- map show exits] 

aWorld = World (Player []) room (mapFromRooms [room, room'])
