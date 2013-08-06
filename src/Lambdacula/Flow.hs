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
import Lambdacula.Reactions

import Control.Lens hiding (Action)
import System.Console.Haskeline

-- Display a prompt, get some input, call some proceeding function
-- to do stuff with it.
promptLoop :: World -> InputT IO ()
promptLoop world = do
    input <- getAction
    let action = processInput input verbs
    either outputStrLn (runAction world) $ quitOrContinue action

-- Ask for an action.
getAction :: InputT IO String
getAction = do
                input <- getInputLine "> "
                case input of
                    Nothing -> return ""
                    Just x -> return x

-- Execute an action
runAction :: World -> State World [String] -> InputT IO ()
runAction world s = do
    liftIO $ printStrs $ fst reaction
    -- runInputT defaultSettings (printStrs (snd reaction))
    promptLoop $ snd reaction 
    where reaction = runState s world 

-- Given the world and an action, do some stuff... and analyze the world.
proceed :: PlayerAction -> WorldAction 
proceed (SimpleAction Zilch) = onlyDisplay "Huh ?"
proceed (SimpleAction Examine) = onlyDo LookAround
proceed (SimpleAction Inventorize) = state $ (,) <$> displayInventory . (map mainName . view playerObjects) <*> id
proceed (SimpleAction Flee) = onlyDo Flight
proceed (Interaction act obj) = getPotentialAction obj act Nothing
proceed (Complex act obj comp) = getPotentialAction obj act (Just comp)
proceed _ = onlyDisplay "Whaaaat ?"

getPotentialAction :: String            -- The main object
                    -> Action           -- The action
                    -> Maybe String     -- Potential interaction
                    -> WorldAction 
getPotentialAction obj act comp = do
                                    objs <- localScope
                                    case findObjectInteraction obj objs act comp of
                                        Left xs -> return xs
                                        Right trio -> processAction trio

-- Check if the proposed action is to quit or to do something.
quitOrContinue :: PlayerAction -> Either String WorldAction
quitOrContinue (SimpleAction QuitGame) = Left "K thx Bye"
quitOrContinue a = Right $ proceed a
