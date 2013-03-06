import Data.List
import Data.Char
import qualified Data.Map as Map
import Action
import Parser
import World

isQuit :: PlayerAction -> Bool
isQuit (SimpleAction QuitGame) = True
isQuit _ = False

proceed :: PlayerAction -> World -> IO ()
proceed (SimpleAction Zilch) w = do
                                putStrLn "Huh ?"
                                promptLoop w
proceed (Interaction t o) w = 
                        do
                            x <- return $ findObject o (currentRoom w)
                            case x of
                                Nothing -> putStrLn $ "There is no " ++ o ++ " here !"
                                Just realObject -> putStrLn (actionOn t realObject)
                            promptLoop w
proceed _ p = promptLoop p

actionOn :: Action -> RoomObject -> String
actionOn act obj = getTextForAction act obj

promptLoop :: World -> IO ()
promptLoop player = do
    putStr "> "
    input <- getLine
    action <- return $ processInput input verbs 
    if (isQuit action) 
        then putStrLn "K Thx Bye"
        else (proceed action player)
    return ()

displayRoom :: Room -> IO ()
displayRoom (Room name desc _ _ _) = do
                                putStrLn $ replicate (length name) '*' 
                                putStrLn (map toUpper name)
                                putStrLn $ replicate (length name) '*' 
                                putStrLn desc

findObject :: String -> Room -> Maybe RoomObject
findObject s room = find (isObject s) (objects room)
    where   isObject :: String -> RoomObject -> Bool
            isObject s o = s `elem` (objectAliases o)

main = do
        displayRoom $ currentRoom aWorld
        promptLoop aWorld

aWorld = World (Player []) room

-- here are some constants to test this code.
speak = Transitive Talk "speak" ["with", "to"] ["about"]
talk = Transitive Talk "talk" ["with", "to"] ["about"]
ask = Transitive Talk "ask" ["about"] []
lookFor = Phrasal Search "look" "for" [] ["in", "with"]
examine = Transitive Examine "examine" [] ["with"]
look = Transitive Examine "look" [] ["with"]
analyze = Transitive Examine "analyze" [] []
quit = Transitive QuitGame "quit" [] []
verbs = [speak, talk, ask, lookFor, examine, look, analyze, quit]


testCube = RoomObject "the test cube" ["Test cube", "cube"] (Map.fromList[(Examine, "A simple test cube. Look, how pretty !"),
                                            (Talk, "You can't talk to a cube, don't be silly"),
                                            (Move, "You push the cube. Happy now ?")]) 


room = Room "The test room" "You are standing in a non-existant place in a virtual world. This is a very good place to hold existential thoughts. Or test the system. But this is more or less the same thing, innit ?" [testCube] [] [] 

