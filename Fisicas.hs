module Fisicas
    (distanceBetween, angleToTarget, deg2rad, rad2deg, subVec, getVertices, dot, sub, perp, isInBounds, mul
    )where

import Entidades

distanceBetween :: Position -> Position -> Distance
distanceBetween (x1, y1) (x2, y2) =
  sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

angleToTarget :: Position -> Position -> Angle
angleToTarget (x1, y1) (x2, y2) =
  atan2 (y2 - y1) (x2 - x1)   -- Se usa atan2 ya que es una comparación de coordenadas

deg2rad :: Angle -> Angle
deg2rad x = x * pi / 180

rad2deg :: Angle -> Angle
rad2deg x = x * 180 / pi

subVec :: Vector -> Vector -> Vector
subVec (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

getVertices :: (Point, Point, Point, Point, Angle) -> [Point]
getVertices ((x1, y1), (x2, y2), (x3, y3), (x4, y4), angulo) =
  let radianes = deg2rad angulo   -- El let es una definición local, solo sirve aquí
      rot (x, y) = (x * cos radianes - y * sin radianes,
                    x * sin radianes + y * cos radianes)
  in [rot (x1, y1), rot (x2, y2), rot (x3, y3), rot (x4, y4)]

dot :: Point -> Point -> Float
dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

sub :: Point -> Point -> Point
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

perp :: Vector -> Vector
perp (x, y) = (-y, x)

isInBounds :: Point -> Size -> Bool
isInBounds (x, y) (width, height) =
  x >= 0 && x <= width && y >= 0 && y <= height

mul :: Size -> Scale -> Size
mul (w,h) (sw, sh) = (w * sw, h * sh)


