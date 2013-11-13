module Lambdacula.WorldBuilder
(
    buildWorld,
    load,
    save
)
where

import Control.Lens
import Data.List
import Data.List.Split
import Control.Applicative
import Data.Graph
import Lambdacula.GameData
import Lambdacula.World


roomsToGraph :: [Room]              -- Every single room
                -> [RoomObject]     -- Every single room object
                -> FullGraphInfo    -- A complete graph
roomsToGraph rms ros = graphFromEdges (map getRoomKeyAndEdges rms)
    where 
        getRoomKeyAndEdges :: Room -> (Room, String, [String])
        getRoomKeyAndEdges r = (r, _roomName r, exitKeys (_roomName r) ros)
        exitKeys :: String -- A room name
                    -> [RoomObject] -- Some objects
                    -> [String] -- A list of destination for the room matching this name
        exitKeys name ros = exitKeys' $ filter (isInRoom name) ros
        exitKeys' :: [RoomObject] -> [String]
        exitKeys' [] = []
        exitKeys' (Exit _ _  _ _ dest:ros) = dest:exitKeys' ros
        exitKeys' (_:ros) = exitKeys' ros


buildWorld :: [Room]            -- A list of rooms
            -> [RoomObject]     -- A list of objects
            -> [ReactionSet]    -- A list of reactions
            -> World            -- Returns a whole world !
buildWorld rooms objects reactions = World (Player firstRoom firstRoom "player") (view _1 rms) objects reactions (view _3 rms) (view _2 rms)
    where 
        firstRoom = head rooms
        rms = roomsToGraph rooms objects

ldclseparator = "<<<>>>"
-- Utilities function to read and show world
showWorld :: World -> String
showWorld w = intercalate ldclseparator worldValues
    where 
        worldValues = show (_player w):[show (_worldObjects w)]

readWorld :: [Room] -> [ReactionSet] -> String -> World
readWorld rooms reacs s = World playerInfo (view _1 rms) objects reacs (view _3 rms) (view _2 rms)
    where 
        splitValues :: [String]
        splitValues = splitOn ldclseparator s
        playerInfo = read . head $ splitValues
        objects = read . last $ splitValues
        rms = roomsToGraph rooms objects

-- Load and save call
save :: String -> World -> IO ()
save name w = writeFile name $ showWorld w

load :: [Room] -> [ReactionSet] -> String -> IO World
load rooms reacs s = fmap (readWorld rooms reacs) (readFile s)
