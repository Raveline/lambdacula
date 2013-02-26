import Data.List
import Data.Char

data PlayerAction = Quit | Action String 
data StatementStyle = Polite | Vulgar | Neutral

vulgarity = ["fuck", "shit", "cunt", "wtf"]
politeness = ["please", "hello", "salutations", "would you mind"]
quit = ["good bye", "quit", "ciao", "see you", "bye bye", "sayonara", "arrivederci"]
vulgarity_reaction = "I can't stand such vulgarity. Please behave !"
politeness_reaction = "How polite of you."


parseLine :: String -> PlayerAction
parseLine s
    | s `elem` quit  = Quit
    | otherwise = Action s

proceed :: PlayerAction -> IO()
proceed (Action s) = 
    do 
        putStrLn $ reactToBasicAction (map toLower s)
        promptLoop

reactToBasicAction :: String -> String
reactToBasicAction s = interpret $ getStatementStyle s
    where 
        interpret :: StatementStyle -> String
        interpret Polite = politeness_reaction 
        interpret Vulgar = vulgarity_reaction 
        interpret Neutral = "You said : " ++ s

getStatementStyle :: String -> StatementStyle
getStatementStyle s 
        | containsWords s vulgarity = Vulgar
        | containsWords s politeness = Polite
        | otherwise = Neutral

containsWords :: String -> [String] -> Bool
containsWords s ws = intersect (words s) ws /= []

isQuit :: PlayerAction -> Bool
isQuit Quit = True
isQuit _ = False

promptLoop :: IO ()
promptLoop = do
    putStr "> "
    input <- getLine
    y <- return(parseLine input)
    if (isQuit y) 
        then putStrLn "K Thx Bye"
        else (proceed y)
    return ()

main = promptLoop
