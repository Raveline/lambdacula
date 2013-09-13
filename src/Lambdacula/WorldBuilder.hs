module Lambdacula.WorldBuilder
(
    buildWorld
)
where

import Data.Graph
import Lambdacula.GameData
import Lambdacula.World
import Control.Lens


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
        exitKeys' ((Exit _ _  _ _ dest):ros) = dest:(exitKeys' ros)
        exitKeys' (_:ros) = exitKeys' ros


buildWorld :: [Room]            -- A list of rooms
            -> [RoomObject]     -- A list of objects
            -> [ReactionSet]    -- A list of reactions
            -> World            -- Returns a whole world !
buildWorld rooms objects reactions = World firstRoom firstRoom (view _1 rms) objects reactions (view _3 rms) (view _2 rms)
    where 
        rms = roomsToGraph rooms objects
        firstRoom = head rooms

