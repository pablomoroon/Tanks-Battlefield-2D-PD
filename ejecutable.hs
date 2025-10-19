{-# LANGUAGE RecordWildCards #-}
module Main where

import Entidades
import Fisicas
import Robot as RB
import Colisiones

import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Interface.Pure.Game as G
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- Estado del mundo
--------------------------------------------------------------------------------
data World = World
  { gs         :: GameState
  , robots     :: [Robot]
  , shots      :: [Proyectil]
  , explosions :: [Explosion]    -- 🔥 nuevas explosiones activas
  , tick       :: Int
  , elapsed    :: Tiempo
  } deriving (Show, Eq)

data Explosion = Explosion
  { expPos  :: Position
  , expTime :: Float
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Constructores rápidos
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
        { name = nm
        , energy = energia
        , range = rango
        , speed = vel
        , tipo  = tipoR
        }
    }

mkProjectile :: Int -> Position -> Vector -> Float -> Int -> Proyectil
mkProjectile pid pos vel dmg owner =
  Objeto
    { objectId      = pid
    , position      = pos
    , velocity      = vel
    , angulo        = 0
    , explosion     = False
    , explosionTime = 0
    , size          = V2 6 6
    , imagenObjeto  = "bullet"
    , extras        = ProyectilData { damage = dmg, ownerId = owner }
    }

--------------------------------------------------------------------------------
-- Aplicar acciones de bots
--------------------------------------------------------------------------------
applyBotActions :: GameState -> [Robot] -> Robot -> [BotAction] -> (Robot, [Proyectil])
applyBotActions _ _ r _
  | not (RB.isRobotAlive r) = (r, [])
applyBotActions _ _ r acts = foldl ejecutar (r, []) acts
  where
    ejecutar (rob, acc) a = case a of
      Stop -> (RB.updateRobotVelocity rob (pure 0), acc)
      Rotate th -> (rob { angulo = rad2deg th }, acc)
      Move p ->
        let v = speed (extras rob) *^ normalize (p ^-^ position rob)
        in (RB.updateRobotVelocity rob v, acc)
      Shoot ->
        let t   = deg2rad (angulo rob)
            dir = V2 (cos t) (sin t)
            v   = (speed (extras rob) * 4) *^ dir
            pid = objectId rob * 100000 + length acc
            spawnPos = position rob ^+^ dir ^* 63
            p'  = mkProjectile pid spawnPos v 10 (objectId rob)
        in (rob, acc ++ [p'])
      Combo xs -> foldl ejecutar (rob, acc) xs

--------------------------------------------------------------------------------
-- Actualización del mundo
--------------------------------------------------------------------------------
stepWorld :: Tiempo -> World -> World
stepWorld dt w@World{..} =
  let
    aliveRobots = filter RB.isRobotAlive robots

    decide r = (r, RB.botDecision tick gs r (filter (\e -> objectId e /= objectId r) aliveRobots))

    (robots', newShots) =
      foldl (\(rs, ps) (r, as) ->
              let (r', ps') =
                    if RB.isRobotAlive r
                      then applyBotActions gs aliveRobots r as
                      else (r, [])   
              in (rs ++ [r'], ps ++ ps'))
            ([], []) (map decide aliveRobots)

    robotsMoved = map (\r -> RB.updatePosition r dt) robots'
    shotsMoved  = map (\p -> p { position = position p ^+^ velocity p ^* dt }) (shots ++ newShots)

    (rrCols, rpCols) = checkCollisions robotsMoved shotsMoved

    stopIds = foldr (\(a,b) acc -> a:b:acc) [] rrCols
    robotsStopped = map
      (\r -> if objectId r `elem` stopIds
             then RB.updateRobotVelocity r (pure 0)
             else r)
      robotsMoved

    hitProjIds = [pid | (_, pid) <- rpCols]

   
    newExplosions =
      [ Explosion (position p) 0
      | (_, pid) <- rpCols
      , p <- shotsMoved
      , objectId p == pid
      ]

    robotsDamaged = foldl aplicarDaño robotsStopped rpCols
    aplicarDaño rs (rid, pid) =
      case filter ((== pid) . objectId) shotsMoved of
        []  -> rs
        p:_ -> map (\r ->
          if objectId r == rid && ownerId (extras p) /= rid
            then r { extras = let rd = extras r
                              in rd { energy = energy rd - damage (extras p) } }
            else r) rs

    shotsRemaining = filter (\p -> objectId p `notElem` hitProjIds
                                && isInBounds (position p) (worldSize gs)) shotsMoved

    robotsClamped = map limitar robotsDamaged
    limitar r =
      let V2 x y = position r
          V2 w h = worldSize gs
      in r { position = V2 (max 0 (min w x)) (max 0 (min h y)) }

    robotsWithExplosions =
      map (\r ->
            if not (RB.isRobotAlive r) && not (explosion r)
              then r { explosion = True, explosionTime = 0 }
              else r) robotsClamped

    robotsExploding =
      map (\r -> if explosion r then r { explosionTime = explosionTime r + dt } else r)
          robotsWithExplosions

    robotsFinal = filter (\r -> not (explosion r && explosionTime r > 1.5)) robotsExploding

  in w { robots     = robotsFinal
       , shots      = shotsRemaining
       , explosions = updateExplosions (explosions ++ newExplosions)
       , tick       = tick + 1
       , elapsed    = elapsed + dt
       }

--------------------------------------------------------------------------------
-- Actualización de explosiones
--------------------------------------------------------------------------------
updateExplosions :: [Explosion] -> [Explosion]
updateExplosions = filter ((<1.0) . expTime) . map avanzar
  where
    avanzar e = e { expTime = expTime e + 0.05 }

--------------------------------------------------------------------------------
-- Render con Gloss
--------------------------------------------------------------------------------
runGloss :: IO ()
runGloss =
  let World{..} = initialWorld
      V2 w h = worldSize gs
  in G.play (G.InWindow "Batalla de Tanques" (round w, round h) (100,100))
             G.white 60 initialWorld drawWorld (\_ w -> w) stepWorld

worldToScreen :: Size -> Position -> (Float, Float)
worldToScreen (V2 w h) (V2 x y) = (x - w/2, y - h/2)

drawWorld :: World -> G.Picture
drawWorld World{..} =
  let V2 w h = worldSize gs
      fondo  = G.Color (G.greyN 0.9) (G.rectangleSolid w h)
      borde  = G.Color G.black (G.rectangleWire (w+4) (h+4))
      dibRob = map (drawRobot (worldSize gs)) robots
      dibPro = map (drawBullet (worldSize gs)) shots
      dibExplRob = map drawExplosion robots
      dibExplImp = map drawImpactExplosion explosions
      hud    = drawHUD (worldSize gs) tick elapsed (length robots)
  in G.Pictures ([fondo,borde] ++ dibRob ++ dibPro ++ dibExplRob ++ dibExplImp ++ [hud])

drawRobot ws r
  | explosion r = G.Blank
  | otherwise =
      let (x,y) = worldToScreen ws (position r)
          ang   = angulo r
          vida  = max 0 (min 100 (energy (extras r)))
          c     = colorTanque r
      in G.Translate x y . G.Rotate (-ang) $ G.Pictures
           [ cuerpoTanque c (0,0)
           , cañon        c (30,0)
           , cabeza       c (-10,0)
           , barraVida    (0,50) vida
           ]

-- explosión al morir (robots)
drawExplosion :: Robot -> G.Picture
drawExplosion r
  | explosion r =
      let (x,y) = worldToScreen (worldSize initialGS) (position r)
          t     = explosionTime r
      in G.Translate x y (explosionPicture t)
  | otherwise = G.Blank
  where initialGS = GameState { worldSize = V2 1000 700, nTanques = 0 }

-- explosión al impacto (balas)
drawImpactExplosion :: Explosion -> G.Picture
drawImpactExplosion (Explosion pos t) =
  let (x, y) = worldToScreen (V2 1000 700) pos
  in G.Translate x y (explosionPicture (t * 1.5))

-- Animación genérica de explosión
explosionPicture :: Float -> G.Picture
explosionPicture t =
  G.Pictures
    [ G.Color (G.withAlpha (1 - t/1.5) G.yellow) (G.circleSolid (5 + 10*t))
    , G.Color (G.withAlpha (0.9 - t/1.5) G.orange) (G.circleSolid (10 + 15*t))
    , G.Color (G.withAlpha (0.7 - t/1.5) G.red) (G.ThickCircle (15 + 20*t) 4)
    ]

--------------------------------------------------------------------------------
-- Dibujos base
--------------------------------------------------------------------------------
cuerpoTanque c (x,y) =
  G.Translate x y $ G.Pictures
    [ G.Color (G.greyN 0.1) (G.rectangleSolid 100 75)
    , G.Color G.black (G.rectangleSolid 106 56)
    , G.Color c (G.rectangleSolid 100 50)
    ]

cabeza c (x,y) = G.Translate x y $ G.Pictures
  [ G.Color G.black (G.circleSolid 17)
  , G.Color c (G.circleSolid 15)
  ]

cañon c (x,y) = G.Translate x y $ G.Pictures
  [ G.Color G.black (G.rectangleSolid 63 8)
  , G.Color c (G.rectangleSolid 60 5)
  ]


barraVida (x,y) v =
  G.Translate x y $ G.Pictures
    [ G.Color G.black (G.rectangleSolid 103 8)
    , G.Translate (-(100 - v)/2) 0 $ G.Color (vidaColor v) (G.rectangleSolid v 5)
    ]

colorTanque r =
  case tipo (extras r) of
    Predeterminado -> G.makeColorI 75 83 32 255  -- verde oliva
    Agresivo  -> G.makeColorI 180 0 0 255   -- rojo brillante

vidaColor v | v > 60 = G.green
            | v > 30 = G.orange
            | otherwise = G.red

drawBullet ws p =
  let (x,y) = worldToScreen ws (position p)
  in G.Translate x y $ G.Color (G.greyN 0.2) (G.circleSolid 4)

drawHUD (V2 w h) tk t n =
  let msg = printf "tick=%d  t=%.2fs  vivos=%d" tk t n
  in G.Translate (-(w/2) + 10) (h/2 - 30) $ G.Scale 0.1 0.1 $ G.Text msg

--------------------------------------------------------------------------------
-- Spawner (círculo)
--------------------------------------------------------------------------------
spawnCircle :: Int -> GameState -> [Robot]
spawnCircle n gs = [ spawn k | k <- [0..n-1] ]
  where
    V2 w h = worldSize gs
    cx = w/2; cy = h/2
    r = min w h * 0.35
    baseSz = V2 40 30
    mitad = n `div` 2
    spawn k =
      let theta = 2*pi*fromIntegral k / fromIntegral n
          x     = cx + r * cos theta
          y     = cy + r * sin theta
          ang   = rad2deg (theta + pi)
          tipoR = if k < mitad then Predeterminado else Agresivo
          nm    = if tipoR == Predeterminado then "BotVerde" else "BotRojo"
      in mkRobot (k+1) nm tipoR (V2 x y) ang baseSz 100 300 60

--------------------------------------------------------------------------------
-- Inicialización
--------------------------------------------------------------------------------
nBots :: Int
nBots = 10

initialWorld :: World
initialWorld =
  let gs0 = GameState { worldSize = V2 1000 700, nTanques = nBots }
  in World { gs = gs0
           , robots = spawnCircle nBots gs0
           , shots = []
           , explosions = []   
           , tick = 0
           , elapsed = 0
           }

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------
main :: IO ()
main = runGloss
