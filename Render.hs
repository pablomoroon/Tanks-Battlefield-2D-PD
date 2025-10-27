{-# OPTIONS_GHC -Wno-type-defaults #-}
module Render
  ( worldToScreen
  , drawRobot, drawExplosion, drawImpactExplosion
  , drawBullet, drawHUDZombies, drawFinJuegoZombies
  ) where

import Entidades
import qualified Graphics.Gloss as G
import Assets (spriteCuerpo, spriteCanion, spriteZombie)

worldToScreen :: Size -> Position -> (Float, Float)
worldToScreen (V2 w h) (V2 x y) = (x - w/2, y - h/2)

drawRobot :: Size -> Robot -> G.Picture
drawRobot ws r
  | explosion r = G.Blank
  | otherwise =
      let (x, y)     = worldToScreen ws (position r)
          angCuerpo  = angulo r
          angCanon   = anguloCanon r

          (spriteBase, escalaSprite) = case tipo (extras r) of
            Humano -> (spriteCuerpo, 0.10)
            Zombie -> (spriteZombie, 0.3)

          escala2 = 0.05

          colorTint = G.makeColorI 255 255 255 255

          vidaActual = energy (extras r)
          vidaMax = case tipo (extras r) of
            Humano -> 200  -- ACTUALIZADO de 100 a 200
            Zombie -> 250  -- ACTUALIZADO de 150 a 250
          porcentajeVida = vidaActual / vidaMax
          anchoBarraMax = 40
          anchoBarraActual = anchoBarraMax * porcentajeVida

          colorVida
            | porcentajeVida > 0.6 = G.green
            | porcentajeVida > 0.3 = G.yellow
            | otherwise = G.red

          barraVida = G.Translate 0 25 $ G.Pictures
            [ G.Color G.black (G.rectangleWire anchoBarraMax 6)
            , G.Translate (- ((anchoBarraMax - anchoBarraActual) / 2)) 0
                (G.Color colorVida (G.rectangleSolid anchoBarraActual 5))
            ]

          dibujoCanon = case tipo (extras r) of
            Humano -> G.Rotate (-(angCanon - 90))
                        (G.Translate 0 0 (G.Scale escala2 escala2 spriteCanion))
            Zombie -> G.Blank

      in G.Translate x y $ G.Pictures
           [ G.Rotate (-angCuerpo)
               (G.Color colorTint (G.Scale escalaSprite escalaSprite spriteBase))
           , dibujoCanon
           , barraVida
           ]

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

drawHUDZombies :: Size -> Int -> Float -> Int -> Int -> G.Picture
drawHUDZombies (V2 w h) tk t humanos zombies =
  G.Translate (-(w/2) + 10) (h/2 - 30) . G.Scale 0.1 0.1 $
    G.Pictures
      [ G.Text ("tick=" ++ show tk ++ "  t=" ++ show (round (t * 100) `div` 100) ++ "s")
      , G.Translate 0 (-15) $ G.Color (G.makeColorI 0 120 255 255) $
          G.Text ("Humanos: " ++ show humanos)
      , G.Translate 0 (-30) $ G.Color (G.makeColorI 0 150 0 255) $
          G.Text ("Zombies: " ++ show zombies)
      ]

drawFinJuegoZombies :: Maybe TipoRobot -> G.Picture
drawFinJuegoZombies mganador =
  let texto = case mganador of
        Just Humano -> "¡LOS HUMANOS SOBREVIVIERON!"
        Just Zombie -> "¡LOS ZOMBIES GANARON!"
        Nothing -> "EMPATE"
      color = case mganador of
        Just Humano -> G.makeColorI 0 120 255 255
        Just Zombie -> G.makeColorI 0 150 0 255
        Nothing -> G.white
  in G.Pictures
     [ G.Color (G.makeColorI 0 0 0 180) (G.rectangleSolid 700 400)
     , G.Color (G.makeColorI 255 215 0 255) (G.rectangleWire 700 400)
     , G.Translate (-250) 100 (G.Scale 0.4 0.4 (G.Color G.yellow (G.Text "¡FIN DEL JUEGO!")))
     , G.Translate (-300)  20 (G.Scale 0.28 0.28 (G.Color color (G.Text texto)))
     ]