module Main 
(
main
)
where

import Lambdacula.GameData
import Lambdacula.WorldBuilder

main = do
        printStrs $ displayRoom (view currentRoom aWorld) (view currentObjects aWorld)
        promptLoop aWorld

-- Load it from WorldBuilder
aWorld = buildWorld ldRooms ldObjects 
