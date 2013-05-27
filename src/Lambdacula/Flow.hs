module Lambdacula.Flow
(
    promptLoop,
    proceed
)
where
import Control.Monad.State
import Control.Applicative
import System.IO

import Lambdacula.GameData
import Lambdacula.Action
import Lambdacula.Parser
import Lambdacula.World
import Lambdacula.Display

import Control.Lens
-- Display a prompt, get some input, call some proceeding function
-- to do stuff with it.
promptLoop :: World -> IO ()
promptLoop world = do
    input <- getAction
    let action = processInput input verbs
    either putStrLn (runAction world) $ quitOrContinue action

-- Ask for an action.
getAction :: IO String
getAction = do
  putStr "> "
  hFlush stdout
  getLine

-- Execute an action
runAction :: World -> State World [String] -> IO ()
runAction world s = do
  printStrs $ fst reaction
  promptLoop $ snd reaction 
  where reaction = runState s world 

-- Given the world and an action, do some stuff... and analyze the world.
proceed :: PlayerAction -> WorldAction 
proceed (SimpleAction Zilch) = singleAnswer "Huh ?"
proceed (SimpleAction Examine) = displayCurrentRoom 
proceed (SimpleAction Inventorize) = state $ (,) <$> displayInventory . (view inventory) <*> id
proceed (Interaction act obj) = do 
                                ros <- use currentObjects
                                case findObjectInteraction obj ros of
                                    Nothing -> singleAnswer $ "There is no " ++ obj ++ " here !"
                                    Just func -> func act
proceed _ = singleAnswer "Whaaaat ?"

-- Check if the proposed action is to quit or to do something.
quitOrContinue :: PlayerAction -> Either String WorldAction
quitOrContinue (SimpleAction QuitGame) = Left "K thx Bye"
quitOrContinue a = Right $ proceed a
