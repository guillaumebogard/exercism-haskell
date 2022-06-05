module LuciansLusciousLasagna (elapsedTimeInMinutes, expectedMinutesInOven, preparationTimeInMinutes) where

expectedMinutesInOven :: Int
expectedMinutesInOven = 40

preparationTimeInMinutes :: Int -> Int
preparationTimeInMinutes nbLayers = 2 * nbLayers

elapsedTimeInMinutes :: Int -> Int -> Int
elapsedTimeInMinutes nbLayers nbMinutes = nbMinutes + preparationTimeInMinutes nbLayers
