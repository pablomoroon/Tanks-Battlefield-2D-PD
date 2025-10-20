{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Main where

import Entidades
import Fisicas
import Robot as RB
import Colisiones
import Assets (obtenerMapa, escalarMapaAlFondo)
import Render (drawRobot, drawExplosion, drawImpactExplosion, drawBullet, drawHUD, drawFinJuego)

import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Interface.Pure.Game as GG
import Data.List (find)

--------------------------------------------------------------------------------
-- ESTADOS DE LA APLICACIÓN
--------------------------------------------------------------------------------

data AppState
  = Menu MenuState
  | Playing World
  deriving (Show, Eq)

data MenuState = MenuState
  { numRobots :: Int
  , mapIndex  :: Int
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- ESTADO DEL JUEGO
--------------------------------------------------------------------------------

data EstadoJuego = Jugando | FinJuego deriving (Show, Eq)

data World = World
  { gs         :: GameState
  , robots     :: [Robot]
  , shots      :: [Proyectil]
  , explosions :: [Explosion]
  , tick       :: Int
  , elapsed    :: Tiempo
  , estado     :: EstadoJuego
  , endTimer   :: Float
  , winnerId   :: Maybe Int
  , mapaActual :: Int
  } deriving (Show, Eq)

data Explosion = Explosion
  { expPos  :: Position
  , expTime :: Float
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- MAIN (INICIO)
--------------------------------------------------------------------------------

main :: IO ()
main =
  GG.play
    (G.InWindow "Batalla de Tanques" (1000, 700) (100, 100))
    G.white
    60
    (Menu (MenuState { numRobots = 10, mapIndex = 0 }))
    drawApp
    handleAppEvent
    stepApp

--------------------------------------------------------------------------------
-- APLICACIÓN GENERAL (MENÚ O JUEGO)
--------------------------------------------------------------------------------

drawApp :: AppState -> G.Picture
drawApp (Menu m)    = drawMenu m
drawApp (Playing w) = drawWorld w

handleAppEvent :: GG.Event -> AppState -> AppState
handleAppEvent (GG.EventKey (GG.MouseButton GG.LeftButton) GG.Down _ (x,y)) (Menu m)
  | dentro (-50) 100 60 40 (x, y) = Menu m { numRobots = max 2  (numRobots m - 1) }
  | dentro   50 100 60 40 (x, y)  = Menu m { numRobots = min 10 (numRobots m + 1) }
  | dentro    0  20 160 40 (x, y) = Menu m { mapIndex  = (mapIndex m + 1) `mod` 3 }
  | dentro    0 (-120) 300 60 (x, y) = Playing (crearMundoDesdeMenu m)
handleAppEvent _ (Playing w) = Playing w
handleAppEvent _ s = s

stepApp :: Float -> AppState -> AppState
stepApp _  (Menu m)    = Menu m
stepApp dt (Playing w) = Playing (stepWorld dt w)

dentro :: Float -> Float -> Float -> Float -> (Float, Float) -> Bool
dentro cx cy w h (x, y) = abs (x - cx) <= w/2 && abs (y - cy) <= h/2

--------------------------------------------------------------------------------
-- MENÚ INICIAL
--------------------------------------------------------------------------------

drawMenu :: MenuState -> G.Picture
drawMenu m =
  G.Pictures
    [ G.Translate (-350) 200 (G.Scale 0.4 0.4 (G.Text "CONFIGURACION DE PARTIDA"))
    , drawLabel (-300) 100 ("Tanques: " ++ show (numRobots m))
    , drawButton (-50) 100 60 40 "-"
    , drawButton   50 100 60 40 "+"
    , drawLabel (-300)  20 ("Mapa: " ++ mapName (mapIndex m))
    , drawButton    0   20 160 40 "Cambiar mapa"
    , drawButton    0 (-120) 300 60 "Comenzar partida"
    ]
  where
    drawLabel x y txt = G.Translate x y (G.Scale 0.18 0.18 (G.Text txt))
    drawButton x y w h txt =
      G.Translate x y $ G.Pictures
        [ G.Color (G.makeColorI 230 230 230 255) (G.rectangleSolid w h)
        , G.Color G.black (G.rectangleWire w h)
        , G.Translate (-(fromIntegral (length txt) * 5)) (-6)
            (G.Scale 0.15 0.15 (G.Text txt))
        ]
    mapName i = case i `mod` 3 of { 0 -> "Bosque"; 1 -> "Desierto"; _ -> "Ciudad" }

--------------------------------------------------------------------------------
-- CREAR MUNDO DESDE EL MENÚ
--------------------------------------------------------------------------------

crearMundoDesdeMenu :: MenuState -> World
crearMundoDesdeMenu MenuState{..} =
  World
    { gs = GameState { worldSize = V2 1000 700, nTanques = numRobots }
    , robots = spawnRandom numRobots (GameState { worldSize = V2 1000 700, nTanques = numRobots })
    , shots = []
    , explosions = []
    , tick = 0
    , elapsed = 0
    , estado = Jugando
    , endTimer = 0
    , winnerId = Nothing
    , mapaActual = mapIndex
    }

--------------------------------------------------------------------------------
-- CONSTRUCTORES
--------------------------------------------------------------------------------

mkRobot :: Int -> String -> TipoRobot -> Position -> Angle -> Size -> Float -> Distance -> Float -> Robot
mkRobot rid nm tipoR pos ang sz energia rango vel =
  Objeto
    { objectId      = rid
    , position      = pos
    , velocity      = pure 0
    , angulo        = ang
    , explosion     = False
    , explosionTime = 0
    , size          = sz
    , imagenObjeto  = nm
    , extras        = RobotData
        { name   = nm, energy = energia, range = rango, speed = vel, tipo = tipoR }
    }

mkProjectile :: Int -> Position -> Vector -> Float -> Int -> Proyectil
mkProjectile pid pos vel dmg owner =
  Objeto
    { objectId = pid, position = pos, velocity = vel, angulo = 0
    , explosion = False, explosionTime = 0, size = V2 6 6, imagenObjeto = "bullet"
    , extras = ProyectilData { damage = dmg, ownerId = owner }
    }

--------------------------------------------------------------------------------
-- LIMITAR POSICIÓN DENTRO DE LOS LÍMITES
--------------------------------------------------------------------------------

clampRobot :: Size -> Robot -> Robot
clampRobot worldSize r = r { position = clampPosition worldSize (size r) (position r) }

--------------------------------------------------------------------------------
-- ACCIONES DE BOTS
--------------------------------------------------------------------------------

applyBotActions :: GameState -> [Robot] -> Robot -> [BotAction] -> (Robot, [Proyectil])
applyBotActions _ _ r _ | not (RB.isRobotAlive r) = (r, [])
applyBotActions _ _ r acts = foldl ejecutar (r, []) acts
  where
    ejecutar (rob, ds) a = case a of
      Stop      -> (RB.updateRobotVelocity rob (pure 0), ds)
      Rotate th -> (rob { angulo = rad2deg th }, ds)
      Move p    -> (RB.updateRobotVelocity
                      rob (speed (extras rob) *^ normalize (p ^-^ position rob)), ds)
      Shoot     -> (rob, ds ++ [ mkProjectile
                                  (objectId rob * 100000 + length ds)
                                  (position rob ^+^ V2 (cos (deg2rad (angulo rob)))
                                                      (sin (deg2rad (angulo rob))) ^* 63)
                                  ((speed (extras rob) * 4) *^ V2 (cos (deg2rad (angulo rob)))
                                                                (sin (deg2rad (angulo rob))))
                                  10 (objectId rob) ])
      Combo xs  -> foldl ejecutar (rob, ds) xs

--------------------------------------------------------------------------------
-- ACTUALIZACIÓN DE EXPLOSIONES
--------------------------------------------------------------------------------

updateExplosions :: [Explosion] -> [Explosion]
updateExplosions = filter ((<1.0) . expTime) . map (\e -> e { expTime = expTime e + 0.05 })

--------------------------------------------------------------------------------
-- LÓGICA PRINCIPAL DEL JUEGO
--------------------------------------------------------------------------------

stepWorld :: Tiempo -> World -> World
stepWorld dt w@World{gs, robots, shots, explosions, tick, elapsed, estado, endTimer, winnerId, mapaActual}
  | estado == FinJuego = w
  | length vivos == 1 =
      if endTimer + dt >= 2.5
         then w { estado = FinJuego
                , winnerId = Just (objectId (head vivos))
                , robots = map (\r -> if objectId r == objectId (head vivos)
                                      then (head vivos){ explosion = True
                                                       , explosionTime = explosionTime (head vivos) + dt }
                                      else r) robots
                , shots = []
                , explosions = updateExplosions explosions
                }
         else w { endTimer = endTimer + dt
                , winnerId = Just (objectId (head vivos))
                , robots = map (\r -> if objectId r == objectId (head vivos)
                                      then (head vivos){ explosion = True
                                                       , explosionTime = explosionTime (head vivos) + dt }
                                      else r) robots
                , shots = updateShots shots
                , explosions = updateExplosions explosions
                }
  | otherwise =
      w { robots     = robotsFinal
        , shots      = shotsRestantes
        , explosions = updateExplosions (explosions ++ nuevasExplosiones)
        , tick       = tick + 1
        , elapsed    = elapsed + dt
        , endTimer   = 0
        , winnerId   = Nothing
        }
  where
    vivos = filter RB.isRobotAlive robots

    updateShots = map (\p -> p { position = position p ^+^ velocity p ^* dt })

    decisiones = [ (r, RB.botDecision tick gs r (filter ((/= objectId r) . objectId) vivos))
                 | r <- vivos ]
    (robotsAccionados, nuevosDisparos) =
      unzip [ applyBotActions gs vivos r a | (r,a) <- decisiones ]

    robotsMovidos = map (clampRobot (worldSize gs) . (`RB.updatePosition` dt)) robotsAccionados

    disparosMovidos =
      [ p { position = position p ^+^ velocity p ^* dt }
      | p <- shots ++ concat nuevosDisparos
      ]

    (colRR, colRP) = checkCollisions robotsMovidos disparosMovidos

    idsParados = concatMap (\(a,b) -> [a,b]) colRR
    robotsParados =
      [ if objectId r `elem` idsParados then RB.updateRobotVelocity r (pure 0) else r
      | r <- robotsMovidos
      ]

    nuevasExplosiones =
      [ Explosion (position p) 0
      | (_, pid) <- colRP
      , p <- disparosMovidos
      , objectId p == pid
      ]

    robotsDañados = foldl aplicarDaño robotsParados colRP
    aplicarDaño rs (rid,pid) = case find ((== pid) . objectId) disparosMovidos of
      Nothing -> rs
      Just p  -> [ if objectId r == rid
                   then r { extras = (extras r) { energy = energy (extras r) - damage (extras p) } }
                   else r
                 | r <- rs ]

    shotsRestantes =
      [ p
      | p <- disparosMovidos
      , objectId p `notElem` map snd colRP
      , isInBounds (position p) (worldSize gs)
      ]

    robotsFinal =
      [ clampRobot (worldSize gs) r
      | r <- map (`RB.updatePosition` dt) robotsDañados
      , not (explosion r && explosionTime r > 1.5)
      ]

--------------------------------------------------------------------------------
-- DIBUJO DEL JUEGO
--------------------------------------------------------------------------------

drawWorld :: World -> G.Picture
drawWorld w
  | estado w == FinJuego =
      G.Pictures (fondo : dibRobots ++ dibExplosiones ++ [mensajeFinal])
  | otherwise =
      G.Pictures (fondo : dibRobots ++ dibBalas ++ dibExplosiones ++ [hud])
  where
    V2 wv hv = worldSize (gs w)
    fondo = escalarMapaAlFondo wv hv (obtenerMapa (mapaActual w))

    dibRobots      = map (drawRobot (worldSize (gs w))) (robots w)
    dibBalas       = map (drawBullet (worldSize (gs w))) (shots w)
    dibExplosiones = map (drawExplosion (worldSize (gs w))) (robots w)
                  ++ [ drawImpactExplosion (worldSize (gs w)) (expPos e) (expTime e)
                     | e <- explosions w
                     ]
    hud            = drawHUD (worldSize (gs w)) (tick w) (elapsed w) (length (robots w))
    mensajeFinal   = drawFinJuego (winnerId w)

--------------------------------------------------------------------------------
-- SPAWN ALEATORIO
--------------------------------------------------------------------------------

spawnRandom :: Int -> GameState -> [Robot]
spawnRandom n gs =
  [ mkRobot
      (k + 1)
      ("Bot" ++ show k)
      (if k < n `div` 2 then Predeterminado else Agresivo)
      (V2 (m + rx k * (w - 2*m))
          (m + ry k * (h - 2*m)))
      (fromIntegral ((seed * (k + 7)) `mod` 360))
      (V2 40 30)
      100
      300
      60
  | k <- [0 .. n - 1]
  , V2 w h <- [worldSize gs]
  ]
  where
    m    = 60 :: Float     -- margen a los bordes
    seed = 72345           -- cambia para otra distribución

    -- frac: parte fraccional
    frac :: Float -> Float
    frac x = x - fromIntegral (floor x :: Int)

    rx :: Int -> Float
    rx i = frac (sin (fromIntegral (seed + 97*i))  * 43758.5453123)
    ry :: Int -> Float
    ry i = frac (sin (fromIntegral (seed + 193*i)) * 24634.6345349)


