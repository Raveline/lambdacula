import Data.Char
import qualified Data.Map as Map
import Action
import Parser
import World
import System.IO

main = do
        displayRoom $ currentRoom aWorld
        promptLoop aWorld

-- Display a prompt, get some input, call some proceeding function
-- to do stuff with it.
promptLoop :: World -> IO ()
promptLoop world = do
    putStr "> "
    hFlush stdout 
    input <- getLine
    let action = processInput input verbs 
    if (isQuit action) 
        then putStrLn "K Thx Bye"
        else
            do
                let reaction = proceed world action
                putStrLn $ fst reaction
                promptLoop $ snd reaction 
    return ()

-- Check if this is a quit action.
isQuit :: PlayerAction -> Bool
isQuit (SimpleAction QuitGame) = True
isQuit _ = False

-- Given the world and an action, do some stuff... and analyze the world.
proceed :: World -> PlayerAction -> (String, World)
proceed w (SimpleAction Zilch) = ("Huh ?", w)
proceed w (SimpleAction Examine) = ("", w)
proceed w (Interaction t o) = case realObject of
                                Nothing ->  ("There is no " ++ o ++ " here !", w)
                                Just obj -> (promptOnAction obj t, w) 
                    where 
                        realObject = findObject o (currentRoom w)
proceed w _ = ("Whaaaat ?", w)

promptOnAction :: RoomObject -> Action -> String
promptOnAction = getTextForAction

-- Display a room description to the player.
displayRoom :: Room -> IO ()
displayRoom (Room name desc _ _ _) = do
                                putStrLn $ replicate (length name) '*' 
                                putStrLn (map toUpper name)
                                putStrLn $ replicate (length name) '*' 
                                putStrLn desc

aWorld = World (Player []) room

-- here are some constants to test this code.
speak = Transitive Talk "speak" ["with", "to"] ["about"]
talk = Transitive Talk "talk" ["with", "to"] ["about"]
ask = Transitive Talk "ask" ["about"] []
lookFor = Phrasal Search "look" "for" [] ["in", "with"]
examine = Transitive Examine "examine" [] ["with"]
look = Transitive Examine "look" [] ["with"] 
analyze = Transitive Examine "analyze" [] []
quit = Transitive QuitGame "quit" [] []
verbs = [speak, talk, ask, lookFor, examine, look, analyze, quit]

testCube = RoomObject "the test cube" ["test cube", "cube"] (Map.fromList[(Examine, "A simple test cube. Look, how pretty !"),
                                            (Talk, "You can't talk to a cube, don't be silly"),
                                            (Move, "You push the cube. Happy now ?")]) 


room = Room "The test room" "You are standing in a non-existant place in a virtual world. This is a very good place to hold existential thoughts. Or test the system. But this is more or less the same thing, innit ?\nThere is a nice **test cube** in the center of the non-space." [testCube] [] [] 

