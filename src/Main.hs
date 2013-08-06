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

import System.Console.Haskeline

main = do
        printStrs $ displayRoom (view currentRoom aWorld) (view currentObjects aWorld)
        runInputT defaultSettings (promptLoop aWorld)

-- Load it from WorldBuilder
aWorld = buildWorld ldRooms ldObjects

