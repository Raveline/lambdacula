module Main 
(
main
)
where

import Lambdacula.GameData
import Lambdacula.WorldBuilder
import Lambdacula.Display
import Lambdacula.World
import Lambdacula.Flow
import Control.Lens hiding(Action)

import Lambdacula.Action
import Lambdacula.ModelShortcuts

main = do
        printStrs $ displayRoom (view currentRoom aWorld) (view currentObjects aWorld)
        promptLoop aWorld

-- Load it from WorldBuilder
aWorld = buildWorld ldRooms ldObjects

