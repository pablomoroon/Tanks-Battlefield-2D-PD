module Robots
  ( detectedAgent, isRobotAlive, countActiveRobots
  , updateRobotVelocity, updateVelocity, updatePosition
  ) where

import Entities
import Physics

detectedAgent :: Robot -> Robot -> Bool
detectedAgent r1 r2 =
  distanceBetween (position r1) (position r2) <= range r1

isRobotAlive :: Robot -> Bool
isRobotAlive r = energy r > 0

countActiveRobots :: [Robot] -> Int
countActiveRobots rs = length [r | r <- rs, isRobotAlive r]

updateRobotVelocity :: Robot -> Velocity -> Robot
updateRobotVelocity r v = r { velocity = v }

updateVelocity :: Action -> Robot -> Robot
updateVelocity (Action s (dx,dy) a _ shoot) r
  | s         = r { velocity = (0,0) }
  | otherwise = r { velocity = (vx + a*dx, vy + a*dy) }
  where (vx,vy) = velocity r

updatePosition :: Robot -> Tiempo -> Robot
updatePosition r t =
  let (x,y)   = position r
      (vx,vy) = velocity r
  in r { position = (x + vx*t, y + vy*t) }