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
import Lambdacula.WorldBuilder
import Lambdacula.Display
import Lambdacula.Reactions

import Control.Lens hiding (Action)
import System.Console.Haskeline

data GameAction = Load | Save | Quit

-- Display a prompt, get some input, call some proceeding function
-- to do stuff with it.
promptLoop :: World -> InputT IO ()
promptLoop world = do
    input <- getAction
    let action = proceed $ processInput input verbs
    either (processGameAction world) (runAction world) action

processGameAction ::  World -> GameAction -> InputT IO ()
processGameAction w Load = do
                            loadedWorld <- liftIO $ load ldRooms ldreactions (_player w)
                            runAction loadedWorld (onlyDisplay "Loaded !") 
processGameAction w Save = do
                            liftIO $ save (_player w) w
                            runAction w (onlyDisplay "Saved !") 
processGameAction w Quit = outputStrLn "Bye !"

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
proceed :: PlayerAction -> Either GameAction WorldAction 
proceed (SimpleAction QuitGame) = Left Quit
proceed (SimpleAction LoadGame) = Left Load
proceed (SimpleAction SaveGame) = Left Save
proceed (SimpleAction Zilch) = Right $ onlyDisplay "Huh ?"
proceed (SimpleAction Examine) = Right $ onlyDo LookAround
proceed (SimpleAction Inventorize) = Right $ state $ (,) <$> displayInventory . map mainName . view playerObjects <*> id
proceed (SimpleAction Flee) = Right $ onlyDo Flight
proceed (Interaction act obj) = Right $ getPotentialAction obj act Nothing
proceed (Complex act obj comp) = Right $ getPotentialAction obj act (Just comp)
proceed _ = Right $ onlyDisplay "Whaaaat ?"

getPotentialAction :: String            -- The main object
                    -> Action           -- The action
                    -> Maybe String     -- Potential interaction
                    -> WorldAction 
getPotentialAction obj act comp = do
                                    objs <- localScope
                                    case findObjectInteraction obj objs act comp of
                                        Left xs -> return xs
                                        Right trio -> processAction trio

