data PlayerAction = Quit | Action String 

parseLine :: String -> PlayerAction
parseLine "quit" = Quit
parseLine s = Action s

proceed :: PlayerAction -> IO()
proceed (Action s) = 
    do 
        putStrLn $ "You said : " ++ s
        promptLoop

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
