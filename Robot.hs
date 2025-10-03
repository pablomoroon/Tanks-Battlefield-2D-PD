module Robots
  ( detectedAgent, isRobotAlive, countActiveRobots
  , updateRobotVelocity, updateVelocity, updatePosition
  ) where

import Entidades
import Fisicas

detectedAgent :: Robot -> Robot -> Bool
detectedAgent r1 r2 =
  distanceBetween (position r1) (position r2) <= range (extras r1)

isRobotAlive :: Robot -> Bool
isRobotAlive r = energy (extras r) > 0

countActiveRobots :: [Robot] -> Int
countActiveRobots rs = length [r | r <- rs, isRobotAlive r]

updateRobotVelocity :: Robot -> Velocity -> Robot
updateRobotVelocity r v = r { velocity = v }

updateVelocity :: Action -> Robot -> Robot
updateVelocity (Action stop (dx, dy) a shoot) r 
    | stop == True = r { velocity = (0, 0)}
    | otherwise = r { velocity = (fst(velocity r) + a*dx, snd(velocity r) + a*dy)}

updatePosition :: Robot -> Tiempo -> Robot
updatePosition r t =
  let (x,y)   = position r
      (vx,vy) = velocity r
  in r { position = (x + vx*t, y + vy*t) }



botDecision :: GameState -> Robot -> [Robot]-> [BotAction]
botDecision
botDecision gs r enemies
| energy (extras r) < 20 = [Stop]   
| null enemies = [Stop]
| otherwise =

      