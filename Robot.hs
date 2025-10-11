module Robot
  ( detectedAgent, isRobotAlive, countActiveRobots
  , updateRobotVelocity, updateVelocity, updatePosition, botDecision
  ) where

import Entidades
import Fisicas
import Control.Applicative (liftA2)

detectedAgent :: Robot -> Robot -> Bool
detectedAgent r1 r2 = distanceBetween (position r1) (position r2) <= range (extras r1)

isRobotAlive :: Robot -> Bool
isRobotAlive r = energy (extras r) > 0

countActiveRobots :: [Robot] -> Int
countActiveRobots = length . filter isRobotAlive

updateRobotVelocity :: Robot -> Velocity -> Robot
updateRobotVelocity r v = r { velocity = v }

updateVelocity :: Action -> Robot -> Robot
updateVelocity (Action stop dir a _shoot) r
  | stop      = r { velocity = pure 0 }
  | otherwise = r { velocity = velocity r ^+^ (dir ^* a) }

updatePosition :: Robot -> Tiempo -> Robot
updatePosition r dt = r { position = position r ^+^ (velocity r ^* dt) }

botDecision :: GameState -> Robot -> [Robot] -> [BotAction]
botDecision _ _ [] = [Stop]
botDecision _ r enemies
  | energy (extras r) < 20 =
      if detectedAgent r enemy
        then [Rotate (angleToTarget (position r) (position enemy)), Shoot]
        else [Rotate (angleToTarget (position r) (position enemy)), Stop]
  | detectedAgent r enemy  = [Rotate (angleToTarget (position r) (position enemy)), Shoot]
  | otherwise              = [Move (position enemy)]
  where
    nearest a b =
      if distanceBetween (position r) (position a) < distanceBetween (position r) (position b)
        then a else b
    enemy = foldl1 nearest enemies
