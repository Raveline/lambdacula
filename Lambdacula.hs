import Data.List
import Data.Char
import qualified Data.Map as Map
import Action
import Parser
import World

isQuit :: PlayerAction -> Bool
isQuit (SimpleAction QuitGame) = True
isQuit _ = False

proceed :: PlayerAction -> Player -> IO ()
proceed (SimpleAction Zilch) p = do
                                putStrLn "Huh ?"
                                promptLoop p
proceed _ p = promptLoop p

promptLoop :: Player -> IO ()
promptLoop player = do
    putStr "> "
    input <- getLine
    action <- return $ processInput input verbs 
    if (isQuit action) 
        then putStrLn "K Thx Bye"
        else (proceed action player)
    return ()


main = promptLoop (Player room [])

-- temporary way of storing the world
data Player = Player { currentRoom :: Room, inventory :: [String] }
    deriving (Show)

-- here are some constants to test this code.
speak = Transitive Talk "speak" ["with", "to"] ["about"]
ask = Transitive Talk "ask" ["about"] []
lookFor = Phrasal Search "look" "for" [] ["in", "with"]
quit = Transitive QuitGame "quit" [] []
verbs = [speak, ask, lookFor, quit]


testCube = RoomObject "Test cube" (Map.fromList[(Examine, "A simple test cube. Look, how pretty !"),
                                            (Talk, "You can't talk to a cube, don't be silly"),
                                            (Move, "You push the cube. Happy now ?")]) 

room = Room "The test room" "You are standing in a non-existant place in a virtual world. This is a very good place to hold existential thoughts. Or test the system. But this is more or less the same thing, innit ?" [testCube] [] [] 

