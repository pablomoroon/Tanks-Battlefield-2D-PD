module Render
  ( worldToScreen
  , drawRobot, drawExplosion, drawImpactExplosion
  , drawBullet, drawHUD, drawFinJuego
  ) where

import Entidades
import qualified Graphics.Gloss as G
import Text.Printf (printf)

worldToScreen :: Size -> Position -> (Float, Float)
worldToScreen (V2 w h) (V2 x y) = (x - w/2, y - h/2)

colorTanque :: Robot -> G.Color
colorTanque r = case tipo (extras r) of
  Predeterminado -> G.makeColorI 75 83 32 255
  Agresivo       -> G.makeColorI 180 0 0 255

vidaColor :: Float -> G.Color
vidaColor v | v > 60 = G.green | v > 30 = G.orange | otherwise = G.red

cuerpoTanque :: G.Color -> (Float,Float) -> G.Picture
cuerpoTanque c (x,y) = G.Translate x y $ G.Pictures
  [ G.Color (G.greyN 0.1) (G.rectangleSolid 100 75)
  , G.Color G.black      (G.rectangleSolid 106 56)
  , G.Color c            (G.rectangleSolid 100 50)
  ]

cabeza :: G.Color -> (Float,Float) -> G.Picture
cabeza c (x,y) = G.Translate x y $ G.Pictures
  [ G.Color G.black (G.circleSolid 17)
  , G.Color c       (G.circleSolid 15)
  ]

canion :: G.Color -> (Float,Float) -> G.Picture
canion c (x,y) = G.Translate x y $ G.Pictures
  [ G.Color G.black (G.rectangleSolid 63 8)
  , G.Color c       (G.rectangleSolid 60 5)
  ]

barraVida :: (Float,Float) -> Float -> G.Picture
barraVida (x,y) v = G.Translate x y $ G.Pictures
  [ G.Color G.black (G.rectangleSolid 103 8)
  , G.Translate (-(100 - v)/2) 0 (G.Color (vidaColor v) (G.rectangleSolid v 5))
  ]

drawRobot :: Size -> Robot -> G.Picture
drawRobot ws r | explosion r = G.Blank
               | otherwise   =
  let (x,y) = worldToScreen ws (position r)
      ang   = angulo r
      vida  = max 0 (min 100 (energy (extras r)))
      c     = colorTanque r
  in G.Translate x y . G.Rotate (-ang) $ G.Pictures
       [ cuerpoTanque c (0,0), canion c (30,0), cabeza c (-10,0), barraVida (0,50) vida ]

explosionPicture :: Float -> G.Picture
explosionPicture t = G.Pictures
  [ G.Color (G.withAlpha (1   - t/1.5) G.yellow) (G.circleSolid (5  + 10*t))
  , G.Color (G.withAlpha (0.9 - t/1.5) G.orange) (G.circleSolid (10 + 15*t))
  , G.Color (G.withAlpha (0.7 - t/1.5) G.red)    (G.ThickCircle (15 + 20*t) 4)
  ]

drawExplosion :: Size -> Robot -> G.Picture
drawExplosion ws r
  | explosion r = let (x,y) = worldToScreen ws (position r)
                  in G.Translate x y (explosionPicture (explosionTime r))
  | otherwise   = G.Blank


drawImpactExplosion :: Size -> Position -> Float -> G.Picture
drawImpactExplosion ws pos t =
  let (x,y) = worldToScreen ws pos
  in G.Translate x y (explosionPicture (t * 1.5))

drawBullet :: Size -> Proyectil -> G.Picture
drawBullet ws p = let (x,y) = worldToScreen ws (position p)
                  in G.Translate x y (G.Color (G.greyN 0.2) (G.circleSolid 4))

drawHUD :: Size -> Int -> Float -> Int -> G.Picture
drawHUD (V2 w h) tk t n =
  G.Translate (-(w/2) + 10) (h/2 - 30) . G.Scale 0.1 0.1 $
    G.Text (printf "tick=%d  t=%.2fs  vivos=%d" tk t n)

drawFinJuego :: Maybe Int -> G.Picture
drawFinJuego mwinner =
  let texto = maybe "Sin ganador" (\i -> "Ganador: Tanque " ++ show i) mwinner
  in G.Pictures
     [ G.Color (G.makeColorI 0 0 0 180) (G.rectangleSolid 600 400)
     , G.Color (G.makeColorI 255 215 0 255) (G.rectangleWire 600 400)
     , G.Translate (-200) 100 (G.Scale 0.4 0.4 (G.Color G.yellow (G.Text "¡VICTORIA!")))
     , G.Translate (-180)  20 (G.Scale 0.28 0.28 (G.Color G.white  (G.Text texto)))
     , G.Translate (-220) (-60) (G.Scale 0.18 0.18 (G.Color (G.greyN 0.8) (G.Text "Presiona 'R' para reiniciar")))
     ]
