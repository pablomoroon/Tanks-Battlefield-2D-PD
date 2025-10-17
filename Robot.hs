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

botDecision :: Int -> GameState -> Robot -> [Robot] -> [BotAction]
botDecision tick _ r [] = [Stop]
botDecision tick _ r enemies =
  let
    nearest a b =
      if distanceBetween (position r) (position a) < distanceBetween (position r) (position b)
         then a else b
    enemy = foldl1 nearest enemies
    pR = position r
    pE = position enemy
    ang = angleToTarget pR pE
    dist = distanceBetween pR pE
    ene = energy (extras r)
    rango = range (extras r)
    dir = normalize (pE ^-^ pR)
    minDist = 150
    maxDist = 0.9 * rango
    shootEvery = 50          -- dispara cada 50 ticks (~0.8s)
    canShoot = tick `mod` shootEvery == 0
    facing = let t = deg2rad (angulo r)
             in V2 (cos t) (sin t) `dot` dir > 0.85
  in
    if ene < 20 then
      [Rotate ang, Move (pR ^-^ dir ^* 80)]           -- huir
    else if dist < minDist then
      [Rotate ang, Move (pR ^-^ dir ^* 60)]           -- alejarse
    else if dist > maxDist then
      [Rotate ang, Move (pR ^+^ dir ^* 120)]          -- acercarse
    else if facing && canShoot then
      [Rotate ang, Shoot]                             -- disparar si puede
    else
      [Rotate ang, Stop]                              -- ajustar orientación