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
        runInputT defaultSettings startUp -- (promptLoop aWorld)

