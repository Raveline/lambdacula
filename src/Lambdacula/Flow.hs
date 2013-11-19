module Lambdacula.Flow
(
    startUp,
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

import System.Directory
data GameAction = Load | Save | Quit

startUp :: InputT IO ()
startUp = do
        liftIO $ putStrLn "Hello ! Welcome to Lambdacula, a Functional Transylvanian Adventure game !"
        x <- getStableInputLine "Would you please tell us your name : "
        knownPlayer <- liftIO $ doesFileExist x
        case knownPlayer of
            True -> do
                w <- loadGame x
                runAction w (onlyDo LookAround)
            False -> newGame x
    where
        getStableInputLine :: String -> InputT IO String
        getStableInputLine s = do
                x <- getInputLine s
                case x of
                    Nothing -> getStableInputLine s
                    Just line -> return line

newGame :: String -> InputT IO ()
newGame playerName = do
            -- printStrs $ displayRoom (view currentRoom w) (view currentObjects w)
            let hello = ["Splendid, " ++ playerName ++ " ! Let's start a game.", "Next time you tell me this name, I'll load your game directly !"]
            let aWorld = buildWorld playerName ldRooms ldObjects ldreactions

            liftIO $ printStrs $ hello
            runAction aWorld (onlyDo LookAround) 

loadGame :: String -> InputT IO World
loadGame name = liftIO $ load ldRooms ldreactions name

-- Display a prompt, get some input, call some proceeding function
-- to do stuff with it.
promptLoop :: World -> InputT IO ()
promptLoop world = do
    input <- getAction
    let action = proceed $ processInput input verbs
    either (processGameAction world) (runAction world) action

processGameAction ::  World -> GameAction -> InputT IO ()
processGameAction w Load = do
                            let name = (_player w)
                            loadedWorld <- loadGame name 
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

