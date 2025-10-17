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
  { gs      :: GameState
  , robots  :: [Robot]
  , shots   :: [Proyectil]
  , tick    :: Int
  , elapsed :: Tiempo
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Constructores rápidos
--------------------------------------------------------------------------------
mkRobot :: Int -> String -> Position -> Angle -> Size -> Float -> Distance -> Float -> Robot
mkRobot rid nm pos ang sz energia rango vel =
  Objeto
    { objectId     = rid
    , position     = pos
    , velocity     = pure 0
    , angulo       = ang
    , explosion    = False
    , size         = sz
    , imagenObjeto = nm
    , extras       = RobotData { name = nm, energy = energia, range = rango, speed = vel }
    }

mkProjectile :: Int -> Position -> Vector -> Float -> Int -> Proyectil
mkProjectile pid pos vel dmg owner =
  Objeto
    { objectId     = pid
    , position     = pos
    , velocity     = vel
    , angulo       = 0
    , explosion    = False
    , size         = V2 6 6
    , imagenObjeto = "bullet"
    , extras       = ProyectilData { damage = dmg, ownerId = owner }
    }

--------------------------------------------------------------------------------
-- Aplicar acciones de bots usando las funciones del módulo Robot
--------------------------------------------------------------------------------
applyBotActions :: GameState -> [Robot] -> Robot -> [BotAction] -> (Robot, [Proyectil])
applyBotActions _ _ r acts = foldl ejecutar (r, [])
  acts
  where
    ejecutar (rob, acc) a = case a of
      Stop ->
        (RB.updateRobotVelocity rob (pure 0), acc)
      Rotate th ->
        (rob { angulo = rad2deg th }, acc)
      Move p ->
        let v = speed (extras rob) *^ normalize (p ^-^ position rob)
        in (RB.updateRobotVelocity rob v, acc)
      Shoot ->
        let t   = deg2rad (angulo rob)
            dir = V2 (cos t) (sin t)
            v   = (speed (extras rob) * 4) *^ dir
            pid = objectId rob * 100000 + length acc
            p'  = mkProjectile pid (position rob ^+^ dir ^* 20) v 10 (objectId rob)
        in (rob, acc ++ [p'])
      Combo xs ->
        foldl ejecutar (rob, acc) xs

--------------------------------------------------------------------------------
-- Un paso del mundo (usando botDecision, Fisicas y Colisiones)
--------------------------------------------------------------------------------
stepWorld :: Tiempo -> World -> World
stepWorld dt w@World{..} =
  let
    -- 1. Decidir acciones de cada robot usando su IA de Robot.hs
    decide r = (r, RB.botDecision tick gs r (filter (\e -> objectId e /= objectId r) robots))

    -- 2. Ejecutar acciones -> obtener robots actualizados y nuevos proyectiles
    (robots', newShots) = foldl (\(rs, ps) (r, as) ->
                                   let (r', ps') = applyBotActions gs robots r as
                                   in (rs ++ [r'], ps ++ ps'))
                                 ([], []) (map decide robots)

    -- 3. Actualizar posiciones con Fisicas.hs
    robotsMoved = map (\r -> RB.updatePosition r dt) robots'
    shotsMoved  = map (\p -> p { position = position p ^+^ velocity p ^* dt }) (shots ++ newShots)

    -- 4. Comprobar colisiones (Colisiones.hs)
    (rrCols, rpCols) = checkCollisions robotsMoved shotsMoved

    -- 5. Resolver colisiones robot↔robot
    stopIds = foldr (\(a,b) acc -> a:b:acc) [] rrCols
    robotsStopped = map
      (\r -> if objectId r `elem` stopIds
             then RB.updateRobotVelocity r (pure 0)
             else r)
      robotsMoved

    -- 6. Aplicar daño de proyectiles
    hitProjIds = [pid | (_, pid) <- rpCols]
    robotsDamaged = foldl aplicarDaño robotsStopped rpCols
    aplicarDaño rs (rid, pid) =
      case filter ((== pid) . objectId) shotsMoved of
        []  -> rs
        p:_ -> map (\r ->
          if objectId r == rid && ownerId (extras p) /= rid
            then r { extras = let rd = extras r
                              in rd { energy = energy rd - damage (extras p) } }
            else r) rs

    -- 7. Filtrar proyectiles activos y mantener robots dentro del mapa
    shotsRemaining = filter (\p -> objectId p `notElem` hitProjIds
                                && isInBounds (position p) (worldSize gs)) shotsMoved
    robotsClamped = map limitar robotsDamaged
    limitar r =
      let V2 x y = position r
          V2 w h = worldSize gs
      in r { position = V2 (max 0 (min w x)) (max 0 (min h y)) }

    -- 8. Filtrar robots vivos y avanzar el tiempo
    vivos = filter RB.isRobotAlive robotsClamped

  in w { robots  = vivos
       , shots   = shotsRemaining
       , tick    = tick + 1
       , elapsed = elapsed + dt }

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
      hud    = drawHUD (worldSize gs) tick elapsed (length robots)
  in G.Pictures ([fondo,borde] ++ dibRob ++ dibPro ++ [hud])

drawRobot :: Size -> Robot -> G.Picture
drawRobot ws r =
  let (x,y) = worldToScreen ws (position r)
      e     = max 0 (min 100 (energy (extras r)))
  in G.Translate x y . G.Rotate (angulo r) $ G.Pictures
       [ G.Color (G.greyN 0.15) (G.rectangleSolid 100 50)
       , G.Color (G.greyN 0.05) (G.rectangleWire 100 50)
       , G.Translate 0 0  (G.Color (G.greyN 0.25) (G.circleSolid 15))
       , G.Translate 40 0 (G.Color (G.greyN 0.25) (G.rectangleSolid 40 6))
       , G.Translate 0 36 (G.Pictures
           [ G.Color G.black (G.rectangleWire 104 8)
           , G.Translate (-(100 - e)/2) 0 (G.Color (vidaColor e) (G.rectangleSolid e 6))
           ])
       ]

vidaColor :: Float -> G.Color
vidaColor v | v > 60 = G.green
            | v > 30 = G.orange
            | otherwise = G.red

drawBullet :: Size -> Proyectil -> G.Picture
drawBullet ws p =
  let (x,y) = worldToScreen ws (position p)
  in G.Translate x y $ G.Color (G.greyN 0.2) (G.circleSolid 4)

drawHUD :: Size -> Int -> Tiempo -> Int -> G.Picture
drawHUD (V2 w h) tk t n =
  let msg = printf "tick=%d  t=%.2fs  vivos=%d" tk t n
  in G.Translate (-(w/2) + 10) (h/2 - 30) $ G.Scale 0.1 0.1 $ G.Text msg

--------------------------------------------------------------------------------
-- Spawner de N tanques en círculo
--------------------------------------------------------------------------------
spawnCircle :: Int -> GameState -> [Robot]
spawnCircle n gs = [ spawn k | k <- [0..n-1] ]
  where
    V2 w h = worldSize gs
    cx = w/2; cy = h/2
    r = min w h * 0.35
    baseSz = V2 40 30
    spawn k =
      let theta = 2*pi*fromIntegral k / fromIntegral n
          x     = cx + r * cos theta
          y     = cy + r * sin theta
          ang   = rad2deg (theta + pi)
      in mkRobot (k+1) ("Bot" ++ show (k+1)) (V2 x y) ang baseSz 100 300 60

--------------------------------------------------------------------------------
-- Inicialización
--------------------------------------------------------------------------------
nBots :: Int
nBots = 6

initialWorld :: World
initialWorld =
  let gs0 = GameState { worldSize = V2 1000 700, nTanques = nBots }
  in World { gs = gs0, robots = spawnCircle nBots gs0, shots = [], tick = 0, elapsed = 0 }

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------
main :: IO ()
main = runGloss
