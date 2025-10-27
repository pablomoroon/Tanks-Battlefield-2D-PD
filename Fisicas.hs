module Fisicas
  ( distanceBetween, angleToTarget, deg2rad, rad2deg
  , subVec, getVertices, dot, sub, perp, isInBounds, mul, normalize, clamp, clampPosition
  ) where

import Entidades
import Control.Applicative (liftA2)

distanceBetween :: Position -> Position -> Distance
distanceBetween p1 p2 = sqrt (dx*dx + dy*dy)
  where
    V2 dx dy = p2 ^-^ p1

angleToTarget :: Position -> Position -> Angle
angleToTarget (V2 x1 y1) (V2 x2 y2) = atan2 (y2 - y1) (x2 - x1)

deg2rad :: Angle -> Angle
deg2rad x = x * pi / 180

rad2deg :: Angle -> Angle
rad2deg x = x * 180 / pi

dot :: Vec2 Float -> Vec2 Float -> Float
dot (V2 x1 y1) (V2 x2 y2) = x1*x2 + y1*y2

perp :: Vec2 Float -> Vec2 Float
perp (V2 x y) = V2 (-y) x

subVec :: Vec2 Float -> Vec2 Float -> Vec2 Float
subVec = (^-^)

sub :: Point -> Point -> Point
sub = (^-^)

isInBounds :: Point -> Size -> Bool
isInBounds (V2 x y) (V2 w h) = x >= 0 && x <= w && y >= 0 && y <= h

mul :: Size -> Scale -> Size
mul (V2 w h) (V2 sw sh) = V2 (w*sw) (h*sh)

normalize :: Vec2 Float -> Vec2 Float
normalize (V2 x y)
  | len == 0  = V2 0 0
  | otherwise = V2 (x/len) (y/len)
  where
    len = sqrt (x*x + y*y)

getVertices :: (Point, Point, Point, Point, Angle) -> [Point]
getVertices (p1, p2, p3, p4, ang) = rot <$> [p1, p2, p3, p4]
  where
    t   = deg2rad ang
    rot (V2 x y) = V2 (x * cos t - y * sin t) (x * sin t + y * cos t)

clamp :: Float -> Float -> Float -> Float
clamp lo hi v = max lo (min hi v)

clampPosition :: Size -> Size -> Position -> Angle -> Position
clampPosition (V2 w h) (V2 rw rh) pos ang =
  let
    halfW = w 
    halfH = h 
    hw = rw / 2
    hh = rh / 2
    rad = deg2rad ang

    rot (V2 x y) = V2 (x * cos rad - y * sin rad)
                      (x * sin rad + y * cos rad)
    esquinas = map (^+^ pos)
      [ rot (V2 (-hw) (-hh))
      , rot (V2 hw (-hh))
      , rot (V2 hw hh)
      , rot (V2 (-hw) hh)
      ]

    xs = map (\(V2 x _) -> x) esquinas
    ys = map (\(V2 _ y) -> y) esquinas

    minX = -halfW
    maxX =  halfW
    minY = -halfH
    maxY =  halfH

    dx
      | minimum xs < minX = minX - minimum xs
      | maximum xs > maxX = maxX - maximum xs
      | otherwise          = 0

    dy
      | minimum ys < minY = minY - minimum ys
      | maximum ys > maxY = maxY - maximum ys
      | otherwise          = 0
  in
    pos ^+^ V2 dx dy