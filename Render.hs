{-# OPTIONS_GHC -Wno-type-defaults #-}
module Render
  ( worldToScreen
  , drawRobot, drawExplosion, drawImpactExplosion
  , drawBullet, drawHUDZombies, drawFinJuegoZombies,drawImpactVeneno,drawVeneno,drawSangreZombie
  ) where

import Entidades
import qualified Graphics.Gloss as G
import Assets (spriteSangre,spriteCuerpo, spriteCanion, spriteZombie,spritesExplosion,spritesVeneno,spriteDanino)

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
            Humano -> (spriteCuerpo, 0.1)
            Zombie -> (spriteZombie, 0.07)

          escala2 = 0.05

          flashIntensity = damageFlash (extras r)
          flashActive = flashIntensity > 0
          
          colorTint = if flashActive
                      then G.makeColorI 255 (round (100 * (1 - flashIntensity / 0.2))) (round (100 * (1 - flashIntensity / 0.2))) 255
                      else G.makeColorI 255 255 255 255

          vidaActual = energy (extras r)
          vidaMax = case tipo (extras r) of
            Humano -> 300
            Zombie -> 350
          porcentajeVida = vidaActual / vidaMax
          
          escudoActual = shield (extras r)
          escudoMax = maxShield (extras r)
          porcentajeEscudo = escudoActual / escudoMax
          
          anchoBarraMax = 40
          anchoBarraVida = anchoBarraMax * porcentajeVida
          anchoBarraEscudo = anchoBarraMax * porcentajeEscudo

          colorVida
            | porcentajeVida > 0.6 = G.green
            | porcentajeVida > 0.3 = G.yellow
            | otherwise = G.red

          barraVida = G.Translate 0 22 $ G.Pictures
            [ G.Color G.black (G.rectangleWire anchoBarraMax 6)
            , G.Translate (- ((anchoBarraMax - anchoBarraVida) / 2)) 0
                (G.Color colorVida (G.rectangleSolid anchoBarraVida 5))
            ]
          
          barraEscudo = G.Translate 0 28 $ G.Pictures
            [ G.Color (G.greyN 0.3) (G.rectangleWire anchoBarraMax 4)
            , G.Translate (- ((anchoBarraMax - anchoBarraEscudo) / 2)) 0
                (G.Color (G.makeColorI 50 150 255 255) (G.rectangleSolid anchoBarraEscudo 3))
            ]
          
          efectoImpacto = if flashActive
                          then G.Color (G.withAlpha flashIntensity G.red) 
                                 (G.ThickCircle 25 3)
                          else G.Blank


          dibujoCuerpo = case tipo (extras r) of
            Humano ->
              G.Rotate (-angCuerpo + 90)
                (G.Color colorTint (G.Scale escalaSprite escalaSprite spriteBase))
            Zombie ->
              G.Rotate (-angCuerpo)
                (G.Color colorTint (G.Scale escalaSprite escalaSprite spriteBase))


          dibujoCanon = case tipo (extras r) of
            Humano -> 
              G.Rotate (-(angCanon - 90))
                (G.Translate 0 0 (G.Scale escala2 escala2 spriteCanion))
            Zombie -> G.Blank

      in G.Translate x y $ G.Pictures
           [ dibujoCuerpo
           , dibujoCanon
           , efectoImpacto
           , barraVida
           , barraEscudo
           ]


explosionPicture :: Float -> G.Picture
explosionPicture t = G.Pictures
  [ G.Color (G.withAlpha (1   - t/1.5) G.yellow) (G.circleSolid (5  + 10*t))
  , G.Color (G.withAlpha (0.9 - t/1.5) G.orange) (G.circleSolid (10 + 15*t))
  , G.Color (G.withAlpha (0.7 - t/1.5) G.red)    (G.ThickCircle (15 + 20*t) 4)
  ]

drawExplosion :: Size -> Robot -> G.Picture
drawExplosion ws r
  | explosion r =
      let (x,y) = worldToScreen ws (position r)
      in G.Translate x y (explosionPicture (explosionTime r))
  | otherwise = G.Blank


drawImpactExplosion :: Size -> Position -> Float -> Float -> G.Picture
drawImpactExplosion ws pos t scale =
  let (x, y) = worldToScreen ws pos
      n = length spritesExplosion
      frameIndex = min (n - 1) (floor (t * fromIntegral n))
      frame = spritesExplosion !! frameIndex
      baseScale = 0.25
  in G.Translate x y $
       G.Scale (baseScale * scale) (baseScale * scale) frame


drawBullet :: Size -> Proyectil -> G.Picture
drawBullet ws p =
  let (x,y) = worldToScreen ws (position p)
      pic = case imagenObjeto p of
              "veneno" -> G.Scale 0.1 0.1 $head spritesVeneno   
              _        -> G.Color (G.greyN 0.2) (G.circleSolid 4)
  in G.Translate x y pic


drawHUDZombies :: Size -> Int -> Float -> Int -> Int -> G.Picture
drawHUDZombies (V2 w h) tk t humanos zombies =
  G.Translate (-(w/2) + 10) (h/2 - 30) $
    G.Scale 0.1 0.1 $
      G.Pictures
        [ G.Color G.white $
            G.Text ("tick=" ++ show tk ++ "  t=" ++ show (round (t * 100) `div` 100) ++ "s")
        , G.Translate 1500 150 $
            G.Color (G.makeColorI 0 120 255 255) $
              G.Text ("Humanos: " ++ show humanos)
        , G.Translate 3000 150 $
            G.Color (G.makeColorI 0 180 0 255) $
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

drawImpactVeneno :: Size -> Position -> Float -> Float -> G.Picture
drawImpactVeneno ws pos t scale =
  let (x, y) = worldToScreen ws pos
      n = length spritesVeneno
      frameIndex = min (n - 1) (floor (t * fromIntegral n))
      frame = spritesVeneno !! frameIndex
      baseScale = 0.18
  in G.Translate x y $
       G.Scale (baseScale * scale) (baseScale * scale) frame

drawVeneno :: Size -> Position -> Float -> Float -> G.Picture
drawVeneno ws pos t scale =
  let (x, y) = worldToScreen ws pos
      n = length spriteDanino
      frameIndex = floor (t * 5) `mod` n       

      frame = spriteDanino !! frameIndex
      baseScale = 0.75
  in G.Translate x y $
       G.Scale (baseScale * scale) (baseScale * scale) frame




drawSangreZombie :: Size -> Position -> Float -> Float -> G.Picture
drawSangreZombie ws pos t scale =
  let (x, y) = worldToScreen ws pos
      n = length spriteSangre
      frameIndex = min (n - 1) (floor (t * fromIntegral n *2))
      frame = spriteSangre !! frameIndex
      baseScale = 0.10
  in G.Translate x y $
       G.Scale (baseScale * scale) (baseScale * scale) frame
