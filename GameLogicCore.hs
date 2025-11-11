{-# LANGUAGE RecordWildCards #-}
module GameLogicCore
  ( -- Creación y control de actores
    mkRobot, mkProjectile
  , clampRobot, applyBotActions
  , updateExplosions
  , aplicarDañoConEscudo
  , recargarEscudos
  , separarRobotsEnColision
  , separarRobotsDeObstaculos
    -- Utilidades de mundo compartidas
  , spawnBando
  , generarObstaculos
  , drawObstaculo
  ) where

import Entidades
import Fisicas hiding (sub)
import Robot as RB
import qualified Data.Map.Strict as Map
import Data.List (find, foldl', partition)
import qualified Graphics.Gloss as G
import Assets
  ( tamSpriteExplosivo, tamSpriteDanino, tamSpriteBloqueante
  , spriteObsBloqueante, spritesBomber
  )
import Render (worldToScreen, drawVeneno)

-- ===== ACTORES =====

mkRobot :: Int -> String -> TipoRobot -> Position -> Angle -> Size -> Float -> Distance -> Float -> Robot
mkRobot rid nm tipoR pos ang sz energia rango vel =
  Objeto
    { objectId      = rid
    , position      = pos
    , velocity      = pure 0
    , angulo        = ang
    , anguloCanon   = ang
    , explosion     = False
    , explosionTime = 0
    , size          = sz
    , imagenObjeto  = nm
    , extras        = RobotData
      { name                 = nm
      , energy               = energia
      , shield               = escudoInicial tipoR
      , maxShield            = escudoInicial tipoR
      , shieldRechargeRate   = velocidadRecarga tipoR
      , shieldRechargeDelay  = 0
      , damageFlash          = 0
      , range                = rango
      , speed                = vel
      , tipo                 = tipoR
      , memTarget            = Nothing
      , memLastSeen          = Nothing
      , memAggroCooldown     = 0
      , memLastPosition      = Nothing
      , memStuckCounter      = 0
      , memLastMoveDir       = Nothing
      , memPositionHistory   = []
      , memFailedDestinations = []
      }
    }
  where
    escudoInicial Humano = 100
    escudoInicial Zombie = 80
    velocidadRecarga Humano = 5.0
    velocidadRecarga Zombie = 4.0

mkProjectile :: Int -> Position -> Vector -> Float -> Int -> String -> TipoRobot -> Proyectil
mkProjectile pid pos vel dmg owner tipoBala tipoOwner =
  Objeto
    { objectId = pid
    , position = pos
    , velocity = vel
    , angulo = 0
    , anguloCanon = 0
    , explosion = False
    , explosionTime = 0
    , size = V2 8 8
    , imagenObjeto = tipoBala
    , extras = ProyectilData { damage = dmg, ownerId = owner, ownerTipo = tipoOwner }
    }

clampRobot :: Size -> Robot -> Robot
clampRobot worldSize r =
  let newPos = clampPosition worldSize (size r) (position r) (angulo r)
      fuera  = newPos /= position r
      newVel = if fuera then pure 0 else velocity r
  in r { position = newPos, velocity = newVel }

applyBotActions :: GameState -> [Robot] -> Robot -> [BotAction] -> (Robot, [Proyectil])
applyBotActions _ _ r _ | not (RB.isRobotAlive r) = (r, [])
applyBotActions _ robots r acts = foldl ejecutar (r, []) acts
  where
    ejecutar (rob, ds) a = case a of
      Stop -> (RB.updateRobotVelocity rob (pure 0), ds)
      Rotate th -> (rob { angulo = rad2deg th }, ds)
      RotateCannon th -> (rob { anguloCanon = rad2deg th }, ds)
      Move p ->
        let targetDir   = normalize (p ^-^ position rob)
            targetSpeed = speed (extras rob)
            newVel      = targetSpeed *^ targetDir
            dist        = distanceBetween (position rob) p
        in if dist > 5
           then (RB.updateRobotVelocity rob newVel, ds)
           else (RB.updateRobotVelocity rob (pure 0), ds)
      Accelerate accel ->
        let ang = deg2rad (angulo rob)
            dir = V2 (cos ang) (sin ang)
        in (RB.updateRobotVelocity rob (velocity rob ^+^ (dir ^* accel)), ds)
      Shoot ->
        let angCanon =
              if tipo (extras rob) == Zombie
              then deg2rad (angulo rob)
              else deg2rad (anguloCanon rob)
            dir      = V2 (cos angCanon) (sin angCanon)
            offset   = 45
            posCanon = position rob ^+^ (dir ^* offset)
            velBala  = (speed (extras rob) * 4) *^ dir
            tipoBala = if tipo (extras rob) == Zombie then "veneno" else "bullet"
            dañoBala = if tipoBala == "veneno" then 25 else 40
            newShot = mkProjectile
                        (objectId rob * 100000 + length ds)
                        posCanon
                        velBala
                        dañoBala
                        (objectId rob)
                        tipoBala
                        (tipo (extras rob))
        in (rob, ds ++ [newShot])
      _ -> (rob, ds)

updateExplosions :: [Explosion] -> [Explosion]
updateExplosions = filter ((<1.2) . expTime) . map (\e -> e { expTime = expTime e + 0.08 })

aplicarDañoConEscudo :: Float -> Robot -> Robot
aplicarDañoConEscudo dano robot =
  let ex = extras robot
      escudoActual = shield ex
      dañoAEscudo = min escudoActual dano
      dañoAVida = max 0 (dano - escudoActual)
      nuevoEscudo = max 0 (escudoActual - dano)
      nuevaVida = energy ex - dañoAVida
      muere = nuevaVida <= 0
  in robot
     { extras = ex
       { shield = nuevoEscudo
       , energy = nuevaVida
       , shieldRechargeDelay = 2.0
       , damageFlash = 0.2
       }
     , explosion = muere
     , explosionTime = if muere then 0 else explosionTime robot
     }

recargarEscudos :: Float -> [Robot] -> [Robot]
recargarEscudos dt robots =
  [ let ex = extras r
        delayActual = shieldRechargeDelay ex
        nuevoDelay = max 0 (delayActual - dt)
        escudoActual = shield ex
        escudoMax = maxShield ex
        recarga = shieldRechargeRate ex
        nuevoEscudo = if nuevoDelay <= 0 && escudoActual < escudoMax
                     then min escudoMax (escudoActual + recarga * dt)
                     else escudoActual
        nuevoFlash = max 0 (damageFlash ex - dt)
    in r { extras = ex
           { shield = nuevoEscudo
           , shieldRechargeDelay = nuevoDelay
           , damageFlash = nuevoFlash
           }
         }
  | r <- robots
  ]

separarRobotsEnColision :: [(Int, Int)] -> [Robot] -> [Robot]
separarRobotsEnColision colisiones robots =
  let robotMap = Map.fromList [(objectId r, r) | r <- robots]
      finalMap = foldl' aplicarSeparacion robotMap colisiones
      aplicarSeparacion m (id1, id2) =
        case (Map.lookup id1 m, Map.lookup id2 m) of
          (Just r1, Just r2) ->
            let (r1', r2') = RB.separarRobots r1 r2
                vel1 = velocity r1'
                vel2 = velocity r2'
                normVel1 = sqrt (vx vel1 * vx vel1 + vy vel1 * vy vel1)
                normVel2 = sqrt (vx vel2 * vx vel2 + vy vel2 * vy vel2)
                factorReduccion = 0.6
                r1'' = if normVel1 > 3 then r1' { velocity = vel1 ^* factorReduccion }
                                       else r1' { velocity = vel1 ^* 0.1 }
                r2'' = if normVel2 > 3 then r2' { velocity = vel2 ^* factorReduccion }
                                       else r2' { velocity = vel2 ^* 0.1 }
            in Map.insert id1 r1'' $ Map.insert id2 r2'' m
          _ -> m
  in Map.elems finalMap

separarRobotsDeObstaculos :: [(Int, Int)] -> [Robot] -> [Obstaculo] -> [Robot]
separarRobotsDeObstaculos colisiones robots obstaculos =
  let robotMap = Map.fromList [(objectId r, r) | r <- robots]
      finalMap = foldl' aplicarSeparacion robotMap colisiones
      aplicarSeparacion m (rid, oid) =
        case (Map.lookup rid m, find ((== oid) . objectId) obstaculos) of
          (Just robot, Just obs) ->
            let pR = position robot
                pO = position obs
                delta = pR ^-^ pO
                V2 wr hr = size robot
                V2 wo ho = size obs
                overlapX = (wr/2 + wo/2) - abs (vx delta)
                overlapY = (hr/2 + ho/2) - abs (vy delta)
            in if overlapX > 0 && overlapY > 0 then
                 let empuje | overlapX < overlapY = V2 (signum (vx delta) * overlapX) 0
                            | otherwise           = V2 0 (signum (vy delta) * overlapY)
                     newPos  = pR ^+^ empuje
                     nuevaVel = velocity robot ^* 0.2
                 in Map.insert rid (robot { position = newPos, velocity = nuevaVel }) m
               else m
          _ -> m
  in Map.elems finalMap

-- ===== UTILIDADES DE MUNDO COMPARTIDAS =====

spawnBando :: Int -> GameState -> Int -> TipoRobot -> Int -> [Robot]
spawnBando n gs seed tipoRobot offsetId =
  [ mkRobot
      (k + offsetId + 1)
      (nombreRobot tipoRobot k)
      tipoRobot
      (V2 (m + rx k * (w - 2*m) * 0.95)
          (m + ry k * (h - 2*m) * 0.95))
      (fromIntegral ((seed * (k + 7)) `mod` 360))
      (sizeRobot tipoRobot)
      (energiaInicial tipoRobot)
      (rangoVision tipoRobot)
      (velocidadMovimiento tipoRobot)
  | k <- [0 .. n - 1]
  , V2 w h <- [worldSize gs]
  ]
  where
    m = 60 :: Float
    frac :: Float -> Float
    frac x = x - fromIntegral (floor x :: Int)
    rx :: Int -> Float
    rx i = frac (sin (fromIntegral (seed + 97*i + offsetId * 1000)) * 43758.5453123)
    ry :: Int -> Float
    ry i = frac (sin (fromIntegral (seed + 193*i + offsetId * 1000)) * 24634.6345349)

    nombreRobot Humano k = "Humano" ++ show (k + 1)
    nombreRobot Zombie k = "Zombie" ++ show (k + 1)

    sizeRobot Humano = V2 35 28
    sizeRobot Zombie = V2 30 30

    energiaInicial Humano = 300
    energiaInicial Zombie = 350

    rangoVision Humano = 500
    rangoVision Zombie = 450

    velocidadMovimiento Humano = 65
    velocidadMovimiento Zombie = 75

generarObstaculos :: Int -> Float -> Float -> [Obstaculo]
generarObstaculos seed worldW worldH =
  let
      margen = 140
      totalBloqueantes = 4
      totalDaninos     = 3
      totalExplosivos  = 10

      esc1 = 0.05
      esc2 = 0.15

      tam Bloqueante =
        let (sw,sh) = tamSpriteBloqueante
        in V2 (sw * 0.18) (sh * 0.18)
      tam Dañino =
        let (sw,sh) = tamSpriteDanino
        in V2 (sw * esc1) (sh * esc1)
      tam Explosivo =
        let (sw,sh) = tamSpriteExplosivo
        in V2 (sw * esc2) (sh * esc2)

      rand :: Int -> Float
      rand n = fromIntegral ((1103515245 * (seed+n) + 12345) `mod` 2147483647) / 2147483647

      seSolapa o1 o2 =
        let V2 w1 h1 = size o1
            V2 w2 h2 = size o2
            V2 x1 y1 = position o1
            V2 x2 y2 = position o2
            margenSeparacion = 80
        in abs (x1-x2) < (w1+w2)/2 + margenSeparacion &&
           abs (y1-y2) < (h1+h2)/2 + margenSeparacion

      mk oid tipo x y =
        Objeto
          { objectId = 10000 + oid
          , position = V2 x y
          , velocity = pure 0
          , angulo = 0
          , anguloCanon = 0
          , explosion = False
          , explosionTime = 0
          , size = tam tipo
          , imagenObjeto =
              case tipo of
                Bloqueante -> "obstaculo_bloqueante"
                Dañino     -> "trampa_simple"
                Explosivo  -> "obstaculo_explosivo"
          , extras =
              ObstaculoData
                tipo
                (case tipo of
                    Bloqueante -> 0
                    Dañino     -> 1
                    Explosivo  -> 100)
                300
                3
                False
                False
                0
                0
          }

      place existentes oid tipo = go 0
        where
          go 600 = existentes
          go k =
            let x = margen + rand (oid*7+k)  * (worldW - 2*margen)
                y = margen + rand (oid*13+k) * (worldH - 2*margen)
                nuevo = mk oid tipo x y
            in if any (seSolapa nuevo) existentes
               then go (k+1)
               else existentes ++ [nuevo]

      idsBloq = [0 .. totalBloqueantes-1]
      idsDan  = [totalBloqueantes .. totalBloqueantes+totalDaninos-1]
      idsExp  = [totalBloqueantes+totalDaninos .. totalBloqueantes+totalDaninos+totalExplosivos-1]

      paso1 = foldl (\acc i -> place acc i Bloqueante) [] idsBloq
      paso2 = foldl (\acc i -> place acc i Dañino)     paso1 idsDan
      paso3 = foldl (\acc i -> place acc i Explosivo)  paso2 idsExp

  in paso3

-- Dibujar obstáculo (compartido)
drawObstaculo :: Size -> Float -> Obstaculo -> G.Picture
drawObstaculo ws t obs =
  let (x, y) = worldToScreen ws (position obs)
      V2 w h = size obs
      ex = extras obs

      spriteBloq = spriteObsBloqueante
      spriteExpl =
        let frame = animFrame ex `mod` length spritesBomber
        in  spritesBomber !! frame

      escalaDanino =
        let (sw, sh) = tamSpriteDanino
            base     = 0.25 :: Float
            sx       = w / (base * sw)
            sy       = h / (base * sh)
        in  min sx sy
  in case tipoObs ex of
       Bloqueante -> G.Translate x y $ G.Scale (w / 1024) (h / 1024) spriteBloq
       Dañino     -> drawVeneno ws (position obs) (t * 1.0) escalaDanino
       Explosivo  -> G.Translate x y $ G.Scale (w / 1024) (h / 1024) spriteExpl
