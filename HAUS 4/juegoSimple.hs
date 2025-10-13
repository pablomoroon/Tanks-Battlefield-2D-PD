import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Char (toLower)


data Mundo = Mundo
  { posX :: Float
  , posY :: Float
  , velX :: Float
  , velY :: Float
  , enSuelo :: Bool
  , tiempo :: Float
  } deriving Show

-- FÍSICAS
gravedad, impulsoSalto, velocidad, sueloY :: Float
gravedad = -700
impulsoSalto = 300
velocidad = 200
sueloY = -200

-- ESTADO INICIAL
estadoInicial :: Mundo
estadoInicial = Mundo 0 sueloY 0 0 True 0

-- Parámetros del personaje
rCabeza, anchoCuerpo, altoCuerpo, anchoPierna, altoPierna :: Float
rCabeza     = 20
anchoCuerpo = 30
altoCuerpo  = 75
anchoPierna = 10
altoPierna  = 40

-- MAIN
main :: IO ()
main = play ventana fondo fps estadoInicial dibujar manejarEvento actualizar
  where
    ventana = InWindow "JUEGO SIMPLE" (800, 600) (100, 100)
    fondo   = white
    fps     = 60

-- Dibujo general
dibujar :: Mundo -> Picture
dibujar e = Pictures
  [ Translate (posX e) (posY e) $ personaje e
  , Color black $ Line [(-400, sueloY), (400, sueloY)]       
  , Color black $ Translate (-350) 250 $ Scale 0.1 0.1 $
      Text ("X: " ++ show (round (posX e)) ++
            " | " ++ (if enSuelo e then "Suelo" else "Aire"))
  ]

-- Dibuja el personaje completo (anclado por los pies)
personaje :: Mundo -> Picture
personaje e = Translate 0 alturaBase $ Pictures
  [ piernas (tiempo e) (velX e)
  , cuerpo
  , cabeza (velX e)
  ]
  where
    alturaBase = altoPierna                -- pies en (0,0), cuerpo encima

-- CABEZA + OJOS
cabeza :: Float -> Picture
cabeza vx
  | abs vx < 1 = cabezaFrente
  | otherwise  = cabezaLado vx
  where
    cabezaFrente = Pictures
      [ Translate 0 (altoCuerpo + rCabeza) $ Color colorPiel $ circleSolid rCabeza
      , Translate (-6) (altoCuerpo + rCabeza + 3) $ Color black $ circleSolid 3
      , Translate (6)  (altoCuerpo + rCabeza + 3) $ Color black $ circleSolid 3
      ]
    cabezaLado v = Pictures
      [ Translate 0 (altoCuerpo + rCabeza) $ Color colorPiel $ circleSolid rCabeza
      , Translate (ojoX v) (altoCuerpo + rCabeza + 3) $ Color black $ circleSolid 3
      ]
    ojoX v | v > 0     = 7
           | otherwise = -7

colorPiel :: Color
colorPiel = makeColorI 247 220 199 255

-- CUERPO
cuerpo :: Picture
cuerpo = Translate 0 (altoCuerpo / 2) $
           Color red (rectangleSolid anchoCuerpo altoCuerpo)

-- PIERNAS ANIMADAS
piernas :: Float -> Float -> Picture
piernas t vx
  | abs vx < 10 = piernasQuietas
  | otherwise   = piernasAndando
  where
    balanceo = 12 * sin (t * 8)   
    caderaY  = 0                  

    piernasAndando = Pictures
      [--IZQUIERDA
        Translate (-anchoPierna*0.7) caderaY $
          Rotate balanceo $
            Translate 0 (-altoPierna/2) $
              Color blue (rectangleSolid anchoPierna altoPierna)
        --DERECHA
      , Translate (anchoPierna*0.7) caderaY $
          Rotate (-balanceo) $
            Translate 0 (-altoPierna/2) $
              Color blue (rectangleSolid anchoPierna altoPierna)
      ]

    piernasQuietas = Pictures
      [ Translate (-anchoPierna*0.7) (-20) $
          Color blue (rectangleSolid anchoPierna altoPierna)
      , Translate (anchoPierna*0.7) (-20) $
          Color blue (rectangleSolid anchoPierna altoPierna)
      ]

-- EVENTOS (teclas)
manejarEvento :: Event -> Mundo -> Mundo
-- Movimiento lateral
manejarEvento (EventKey (SpecialKey KeyLeft) Down _ _) e = e { velX = -velocidad }
manejarEvento (EventKey (SpecialKey KeyRight) Down _ _) e = e { velX =  velocidad }
manejarEvento (EventKey (Char 'a') Down _ _) e = e { velX = -velocidad }
manejarEvento (EventKey (Char 'd') Down _ _) e = e { velX =  velocidad }

-- Soltar teclas → parar movimiento
manejarEvento (EventKey (SpecialKey KeyLeft)  Up _ _) e | velX e < 0 = e { velX = 0 }
manejarEvento (EventKey (SpecialKey KeyRight) Up _ _) e | velX e > 0 = e { velX = 0 }
manejarEvento (EventKey (Char 'a') Up _ _) e | velX e < 0 = e { velX = 0 }
manejarEvento (EventKey (Char 'd') Up _ _) e | velX e > 0 = e { velX = 0 }

-- Salto
manejarEvento (EventKey (SpecialKey KeyUp) Down _ _) e
  | enSuelo e = e { velY = impulsoSalto, enSuelo = False }
  | otherwise = e
manejarEvento (EventKey (Char 'w') Down _ _) e
  | enSuelo e = e { velY = impulsoSalto, enSuelo = False }
  | otherwise = e

manejarEvento _ e = e


actualizar :: Float -> Mundo -> Mundo
actualizar dt e
  | y' <= sueloY = e { posX = x', posY = sueloY, velY = 0, enSuelo = True, tiempo = tiempo e + dt }
  | otherwise    = e { posX = x', posY = y', velY = vy', enSuelo = False, tiempo = tiempo e + dt }
  where
    vx' = velX e
    vy' = velY e + gravedad * dt
    x'  = posX e + vx' * dt
    y'  = posY e + vy' * dt
