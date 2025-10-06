module Colisiones
  ( checkCollision
  , detectRobotProjectileCollisions
  , detectRobotRobotCollisions
  , checkCollisions
  ) where

import Entidades
import Fisicas
import Data.List (tails)

------------------------------------------------------------
-- === Funciones geométricas auxiliares ===
------------------------------------------------------------
projectPolygon :: [Point] -> Vector -> (Float, Float)
projectPolygon verts axis = (minimum values, maximum values)
  where
    values = [dot v axis | v <- verts]

overlaps :: (Float, Float) -> (Float, Float) -> Bool
overlaps (minA,maxA) (minB,maxB) =
  not (maxA < minB || maxB < minA)

satCollision :: [Point] -> [Point] -> Bool
satCollision verts1 verts2 =
  all checkAxis allAxes
  where
    allAxes = axes verts1 ++ axes verts2
    axes poly = [perp (sub v2 v1) | (v1,v2) <- edges poly]
    edges poly = zip poly (tail poly ++ [head poly])
    normalize (x,y)
      | len == 0  = (0,0)
      | otherwise = (x/len, y/len)
      where len = sqrt (x*x + y*y)
    checkAxis axis = overlaps (projectPolygon verts1 nAxis)
                               (projectPolygon verts2 nAxis)
      where nAxis = normalize axis

------------------------------------------------------------
-- === Conversión de objetos a sus vértices reales ===
------------------------------------------------------------
objectVertices :: Objeto a -> [Point]
objectVertices obj =
  [ (vx + x, vy + y) | (vx, vy) <- rotated ]
  where
    (x, y)   = position obj
    (w, h)   = size obj
    hw       = w / 2
    hh       = h / 2
    baseVerts = [(-hw, -hh), (hw, -hh), (hw, hh), (-hw, hh)]
    rotated   = getVertices (baseVerts !! 0, baseVerts !! 1, baseVerts !! 2, baseVerts !! 3, angulo obj)

------------------------------------------------------------
-- === Detección de colisiones ===
------------------------------------------------------------
checkCollision :: Objeto a -> Objeto b -> Bool
checkCollision o1 o2 =
  satCollision (objectVertices o1) (objectVertices o2)

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
