module Colisiones
  ( checkCollision
  , detectRobotProjectileCollisions
  , detectRobotRobotCollisions
  , checkCollisions
  ) where

import Entidades
import Fisicas
import Data.List (tails)

projectPolygon :: [Point] -> Vector -> (Float, Float)
projectPolygon verts axis = (minimum values, maximum values)
  where
    values = [dot v axis | v <- verts]

overlaps :: (Float, Float) -> (Float, Float) -> Bool
overlaps (minA, maxA) (minB, maxB) = not (maxA < minB || maxB < minA)

objectVertices :: Objeto a -> [Point]
objectVertices obj = (^+^ V2 x y) <$> rotated
  where
    V2 x y = position obj
    V2 w h = size obj
    hw  = w / 2
    hh  = h / 2
    base = [ V2 (-hw) (-hh), V2 hw (-hh), V2 hw hh, V2 (-hw) hh ]
    rotated = getVertices (base !! 0, base !! 1, base !! 2, base !! 3, angulo obj)

axes :: [Point] -> [Vector]
axes poly = [ perp (v2 ^-^ v1) | (v1, v2) <- edges poly ]

edges :: [Point] -> [(Point, Point)]
edges poly = zip poly (tail poly ++ [head poly])

normalizeSafe :: Vector -> Vector
normalizeSafe v
  | nv == V2 0 0 = V2 1 0
  | otherwise    = nv
  where
    nv = normalize v

checkCollision :: Objeto a -> Objeto b -> Bool
checkCollision o1 o2 = all checkAxis allAxes
  where
    verts1  = objectVertices o1
    verts2  = objectVertices o2
    allAxes = axes verts1 ++ axes verts2
    checkAxis axis =
      let nAxis = normalizeSafe axis
      in overlaps (projectPolygon verts1 nAxis) (projectPolygon verts2 nAxis)

detectRobotProjectileCollisions :: [Robot] -> [Proyectil] -> [(Int, Int)]
detectRobotProjectileCollisions robots projectiles =
  [ (objectId r, objectId p)
  | r <- robots
  , p <- projectiles
  , checkCollision r p
  ]

detectRobotRobotCollisions :: [Robot] -> [(Int, Int)]
detectRobotRobotCollisions robots =
  [ (objectId r1, objectId r2)
  | (r1:rest) <- tails robots
  , r2 <- rest
  , checkCollision r1 r2
  ]

checkCollisions :: [Robot] -> [Proyectil] -> ([(Int, Int)], [(Int, Int)])
checkCollisions robots projectiles =
  (detectRobotRobotCollisions robots, detectRobotProjectileCollisions robots projectiles)
