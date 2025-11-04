{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Main where

import Entidades
import Fisicas hiding (sub)
import Robot as RB
import Colisiones
import Assets (obtenerMapa, escalarMapaAlFondo,
               spriteObsBloqueante, spriteDanino, spritesBomber,tamSpriteExplosivo,
               tamSpriteDanino,tamSpriteBloqueante)
import Render (drawRobot, drawExplosion, drawImpactExplosion, drawBullet, drawHUDZombies, drawFinJuegoZombies, worldToScreen)

import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Interface.Pure.Game as GG
import Data.List (find, foldl', partition)
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
-- ESTADOS PRINCIPALES
--------------------------------------------------------------------------------

data AppState
  = Menu MenuState
  | Playing World
  deriving (Show, Eq)

data MenuState = MenuState
  { numHumanos :: Int
  , numZombies :: Int
  , mapIndex   :: Int
  , gameSeed   :: Int
  , hoveredBtn :: Maybe MenuOption
  } deriving (Show, Eq)

data MenuOption
  = BtnHumanosMinus | BtnHumanosPlus
  | BtnZombiesMinus | BtnZombiesPlus
  | BtnMapa
  | BtnStart
  deriving (Eq, Show, Enum, Bounded)

data EstadoJuego = Jugando | FinJuego deriving (Show, Eq)

data World = World
  { gs         :: GameState
  , robots     :: [Robot]
  , shots      :: [Proyectil]
  , explosions :: [Explosion]
  , obstaculos :: [Obstaculo]  
  , tick       :: Int
  , elapsed    :: Tiempo
  , estado     :: EstadoJuego
  , endTimer   :: Float
  , ganador    :: Maybe TipoRobot
  , mapaActual :: Int
  } deriving (Show, Eq)

data Explosion = Explosion
  { expPos   :: Position
  , expTime  :: Float
  , expScale :: Float  -- Escala de la explosión (1.0 normal, >1.0 más grande)
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------------------
main :: IO ()
main =
  GG.play
    G.FullScreen
    (G.makeColorI 15 15 25 255)
    60
    (Menu initialMenu)
    drawApp
    handleAppEvent
    stepApp

--------------------------------------------------------------------------------
-- MENÚ INICIAL
--------------------------------------------------------------------------------

initialMenu :: MenuState
initialMenu =
  MenuState
    { numHumanos = 5
    , numZombies = 5
    , mapIndex   = 0
    , gameSeed   = 0
    , hoveredBtn = Nothing
    }

--------------------------------------------------------------------------------
-- EVENTOS DEL MENÚ
--------------------------------------------------------------------------------

handleAppEvent :: GG.Event -> AppState -> AppState
handleAppEvent (GG.EventKey (GG.MouseButton GG.LeftButton) GG.Down _ (x, y)) (Menu m) =
  case detectarBoton (x, y) of
    Just btn -> handleButtonClick btn m
    Nothing  -> Menu m
handleAppEvent (GG.EventMotion (x, y)) (Menu m) =
  Menu m { hoveredBtn = detectarBoton (x, y) }
handleAppEvent (GG.EventKey (GG.SpecialKey GG.KeyEsc) GG.Down _ _) (Playing _) =
  Menu initialMenu
handleAppEvent _ s = s

handleButtonClick :: MenuOption -> MenuState -> AppState
handleButtonClick btn m =
  case btn of
    BtnHumanosMinus -> Menu m { numHumanos = max 1 (numHumanos m - 1) }
    BtnHumanosPlus  -> Menu m { numHumanos = min 10 (numHumanos m + 1) }
    BtnZombiesMinus -> Menu m { numZombies = max 1 (numZombies m - 1) }
    BtnZombiesPlus  -> Menu m { numZombies = min 10 (numZombies m + 1) }
    BtnMapa         -> Menu m { mapIndex = (mapIndex m + 1) `mod` 3 }
    BtnStart        -> Playing (crearMundoDesdeMenu m)

-- Botones del menu
detectarBoton :: (Float, Float) -> Maybe MenuOption
detectarBoton (x, y)
  | dentro (-270, 40) 70 50 = Just BtnHumanosMinus
  | dentro (-130, 40) 70 50 = Just BtnHumanosPlus
  | dentro (160, 40) 70 50 = Just BtnZombiesMinus
  | dentro (300, 40) 70 50 = Just BtnZombiesPlus
  | dentro (0, -40) 300 60 = Just BtnMapa
  | dentro (20, -130) 500 80 = Just BtnStart
  | otherwise = Nothing
  where
    dentro (cx, cy) w h = abs (x - cx) <= w/2 && abs (y - cy) <= h/2


--------------------------------------------------------------------------------
-- DIBUJO DEL MENÚ CENTRADO
--------------------------------------------------------------------------------
drawWorld :: World -> G.Picture
drawWorld w
  | estado w == FinJuego =
      G.Pictures (fondo : dibObstaculos ++ dibRobots ++ dibExplosiones ++ [mensajeFinal])
  | otherwise =
      G.Pictures (fondo : dibObstaculos ++ dibRobots ++ dibBalas ++ dibExplosiones ++ [hud])
  where
    V2 wv hv = worldSize (gs w)
    fondo = escalarMapaAlFondo wv hv (obtenerMapa (mapaActual w))

    dibObstaculos = map (drawObstaculo (worldSize (gs w))) (obstaculos w)  
    dibRobots = map (drawRobot (worldSize (gs w))) (robots w)
    dibBalas = map (drawBullet (worldSize (gs w))) (shots w)
    dibExplosiones =
      map (drawExplosion (worldSize (gs w))) (robots w)
      ++ [ drawImpactExplosion (worldSize (gs w)) (expPos e) (expTime e) (expScale e)
         | e <- explosions w
         ]

    humanosVivos = length $ filter (\r -> RB.isRobotAlive r && tipo (extras r) == Humano) (robots w)
    zombiesVivos = length $ filter (\r -> RB.isRobotAlive r && tipo (extras r) == Zombie) (robots w)

    hud = drawHUDZombies (worldSize (gs w)) (tick w) (elapsed w) humanosVivos zombiesVivos

    mensajeFinal = drawFinJuegoZombies (ganador w)

drawApp :: AppState -> G.Picture
drawApp (Menu m)    = drawMenu m
drawApp (Playing w) = drawWorld w

drawMenu :: MenuState -> G.Picture
drawMenu MenuState{..} =
  let
    panelW = 850
    panelH = 550
    baseColor = G.makeColorI 25 35 55 255

    titulo =
      G.Translate (-330) 170 $
        G.Scale 0.4 0.4 $
          G.Color (G.makeColorI 255 230 100 255) (G.Text "HUMANOS vs ZOMBIES")

    sub =
      G.Translate (-120) 130 $
        G.Scale 0.22 0.22 $
          G.Color (G.greyN 0.7) (G.Text "Selecciona opciones:")

    boton txt (cx, cy) w h col =
      G.Translate cx cy $
        G.Pictures
          [ G.Color (G.withAlpha 0.4 col) (G.rectangleSolid w h)
          , G.Color (G.withAlpha 0.9 col) (G.rectangleWire w h)
          , G.Translate (-fromIntegral (length txt) * 7) (-8)
              $ G.Scale 0.25 0.25 (G.Color G.white (G.Text txt))
          ]
  in
  G.Pictures
    [ G.Color (G.makeColorI 10 10 25 255) (G.rectangleSolid 1280 800)
    , G.Color baseColor (G.rectangleSolid panelW panelH)
    , G.Color (G.withAlpha 0.6 G.white) (G.rectangleWire panelW panelH)
    , titulo
    , sub
    , G.Translate (-260) 80 $ G.Scale 0.25 0.25 $
        G.Color (G.makeColorI 80 160 255 255) (G.Text "Humanos")
    , boton "-" (-270, 40) 70 50 (G.makeColorI 70 150 255 255)
    , G.Translate (-200) 40 $ G.Scale 0.3 0.3 (G.Color G.white (G.Text (show numHumanos)))
    , boton "+" (-130, 40) 70 50 (G.makeColorI 70 150 255 255)
    , G.Translate (170) 80 $ G.Scale 0.25 0.25 $
        G.Color (G.makeColorI 100 255 100 255) (G.Text "Zombies")
    , boton "-" (160, 40) 70 50 (G.makeColorI 100 255 100 255)
    , G.Translate (230) 40 $ G.Scale 0.3 0.3 (G.Color G.white (G.Text (show numZombies)))
    , boton "+" (300, 40) 70 50 (G.makeColorI 100 255 100 255)
    , boton ("Mapa: " ++ nombreMapa mapIndex) (0, -40) 300 60 (G.makeColorI 160 120 255 255)
    , boton "COMENZAR PARTIDA" (20, -130) 500 80 (G.makeColorI 255 100 80 255)
    ]

nombreMapa :: Int -> String
nombreMapa i = case i `mod` 3 of
  0 -> "Bosque"
  1 -> "Desierto"
  _ -> "Ciudad"

--------------------------------------------------------------------------------
-- LOOP PRINCIPAL
--------------------------------------------------------------------------------

stepApp :: Float -> AppState -> AppState
stepApp dt (Menu m)    = Menu m { gameSeed = gameSeed m + 1 }
stepApp dt (Playing w) = Playing (stepWorld dt w)

--------------------------------------------------------------------------------
-- CREAR MUNDO DESDE EL MENÚ
--------------------------------------------------------------------------------

crearMundoDesdeMenu :: MenuState -> World
crearMundoDesdeMenu MenuState{..} =
  let
    worldW = 1280
    worldH = 780
    totalTanques = numHumanos + numZombies
    gs0 = GameState { worldSize = V2 worldW worldH, nTanques = totalTanques }
    seed = gameSeed * 97 + mapIndex * 17
    humanos = spawnBando numHumanos gs0 seed Humano 0
    zombies = spawnBando numZombies gs0 (seed + 999) Zombie numHumanos
    obstaculosGenerados = generarObstaculos seed worldW worldH
  in
  World
    { gs = gs0
    , robots = humanos ++ zombies
    , shots = []
    , explosions = []
    , obstaculos = obstaculosGenerados  
    , tick = 0
    , elapsed = 0
    , estado = Jugando
    , endTimer = 0
    , ganador = Nothing
    , mapaActual = mapIndex
    }

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
      { name   = nm
      , energy = energia
      , shield = escudoInicial tipoR                    
      , maxShield = escudoInicial tipoR                 
      , shieldRechargeRate = velocidadRecarga tipoR     
      , shieldRechargeDelay = 0                         
      , damageFlash = 0                                 
      , range  = rango
      , speed  = vel
      , tipo   = tipoR
      , memTarget = Nothing
      , memRole = Nothing
      , memLastSeen = Nothing
      , memAggroCooldown = 0
      , memLastPosition = Nothing  
      , memStuckCounter = 0        
      , memLastMoveDir = Nothing   
      , memPositionHistory = []    
      , memFailedDestinations = [] 
      }
    }
  where
    escudoInicial Humano = 100
    escudoInicial Zombie = 80
    velocidadRecarga Humano = 5.0  -- Recarga 5 puntos por tick después del delay
    velocidadRecarga Zombie = 4.0  -- Recarga 4 puntos por tick después del delay

mkProjectile :: Int -> Position -> Vector -> Float -> Int -> Proyectil
mkProjectile pid pos vel dmg owner =
  Objeto
    { objectId = pid, position = pos, velocity = vel
    , angulo = 0, anguloCanon = 0
    , explosion = False, explosionTime = 0
    , size = V2 8 8
    , imagenObjeto = "bullet"
    , extras = ProyectilData { damage = dmg, ownerId = owner }
    }

clampRobot :: Size -> Robot -> Robot
clampRobot worldSize r =
  let newPos = clampPosition worldSize (size r) (position r) (angulo r)
      fuera  = newPos /= position r
      newVel = if fuera then pure 0 else velocity r
  in r { position = newPos, velocity = newVel }

applyBotActions :: GameState -> [Robot] -> Robot -> [BotAction] -> (Robot, [Proyectil])
applyBotActions _ _ r _ | not (RB.isRobotAlive r) = (r, [])
applyBotActions _ _ r acts = foldl ejecutar (r, []) acts
  where
    ejecutar (rob, ds) a = case a of
      Stop            -> (RB.updateRobotVelocity rob (pure 0), ds)
      Rotate th       -> (rob { angulo = rad2deg th }, ds)
      RotateCannon th -> (rob { anguloCanon = rad2deg th }, ds)
      Move p          ->
        (RB.updateRobotVelocity
          rob (speed (extras rob) *^ normalize (p ^-^ position rob)), ds)
      Accelerate accel ->
        let ang = deg2rad (angulo rob)
            dir = V2 (cos ang) (sin ang)
        in (RB.updateRobotVelocity rob (velocity rob ^+^ (dir ^* accel)), ds)

      SetTarget mId ->
        let ex = extras rob in (rob { extras = ex { memTarget = mId } }, ds)

      SetRole mrole ->
        let ex = extras rob in (rob { extras = ex { memRole = mrole } }, ds)

      UpdateLastSeen pos ->
        let ex = extras rob in (rob { extras = ex { memLastSeen = Just pos } }, ds)

      SetAggroCooldown n ->
        let ex = extras rob in (rob { extras = ex { memAggroCooldown = n } }, ds)

      UpdateStuckState pos counter dir ->  
        let ex = extras rob 
        in (rob { extras = ex { memLastPosition = Just pos
                              , memStuckCounter = counter
                              , memLastMoveDir = dir } }, ds)

      UpdatePositionHistory pos ->  
        let ex = extras rob
            history = memPositionHistory ex
            newHistory = take 10 (pos : history)  -- Mantener últimas 10 posiciones
        in (rob { extras = ex { memPositionHistory = newHistory } }, ds)
      
      MarkFailedDestination pos ->  
        let ex = extras rob
            failed = memFailedDestinations ex
            newFailed = take 5 (pos : failed)  -- Mantener últimos 5 destinos fallidos
        in (rob { extras = ex { memFailedDestinations = newFailed } }, ds)

      Shoot ->
        if tipo (extras rob) == Humano
        then
          let angCanon = deg2rad (anguloCanon rob)
              offset = 45
              dir = V2 (cos angCanon) (sin angCanon)
              posCanon = position rob ^+^ (dir ^* offset)
              velBala = (speed (extras rob) * 4) *^ dir
              newShot = mkProjectile
                (objectId rob * 100000 + length ds)
                posCanon
                velBala
                8  -- Reducido de 12 a 8
                (objectId rob)
          in (rob, ds ++ [newShot])
        else (rob, ds)

      Combo xs -> foldl ejecutar (rob, ds) xs

updateExplosions :: [Explosion] -> [Explosion]
updateExplosions = filter ((<1.2) . expTime) . map (\e -> e { expTime = expTime e + 0.08 })  -- Duración reducida a 1.2, velocidad aumentada a 0.08 para más rapidez

-- Aplicar daño considerando el escudo primero
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
       , shieldRechargeDelay = 2.0  -- 2 segundos de delay antes de recargar
       , damageFlash = 0.2  -- Activar flash de daño por 0.2 segundos
       }
     , explosion = muere
     , explosionTime = if muere then 0 else explosionTime robot
     }

-- Recargar escudos con el tiempo
recargarEscudos :: Float -> [Robot] -> [Robot]
recargarEscudos dt robots = 
  [ let ex = extras r
        delayActual = shieldRechargeDelay ex
        nuevoDelay = max 0 (delayActual - dt)
        escudoActual = shield ex
        escudoMax = maxShield ex
        recarga = shieldRechargeRate ex
        -- Solo recarga si el delay llegó a 0 y el escudo no está lleno
        nuevoEscudo = if nuevoDelay <= 0 && escudoActual < escudoMax
                     then min escudoMax (escudoActual + recarga * dt)
                     else escudoActual
        --  Reducir el timer del flash de daño
        nuevoFlash = max 0 (damageFlash ex - dt)
    in r { extras = ex 
           { shield = nuevoEscudo
           , shieldRechargeDelay = nuevoDelay
           , damageFlash = nuevoFlash
           }
         }
  | r <- robots
  ]

-- FUNCIÓN: Separar robots que colisionan
separarRobotsEnColision :: [(Int, Int)] -> [Robot] -> [Robot]
separarRobotsEnColision colisiones robots =
  let robotMap = Map.fromList [(objectId r, r) | r <- robots]

      -- Aplicar todas las separaciones
      finalMap = foldl' aplicarSeparacion robotMap colisiones

      aplicarSeparacion m (id1, id2) =
        case (Map.lookup id1 m, Map.lookup id2 m) of
          (Just r1, Just r2) ->
            let (r1', r2') = RB.separarRobots r1 r2
                -- Cambiar dirección de movimiento al chocar (invertir velocidad)
                delta = position r1 ^-^ position r2
                norm = sqrt (vx delta * vx delta + vy delta * vy delta)
                dir1 = if norm > 0 then delta ^* (1 / norm) else V2 1 0
                dir2 = dir1 ^* (-1)
                speed = 80  -- Velocidad directa
                r1'' = r1' { velocity = dir1 ^* speed }
                r2'' = r2' { velocity = dir2 ^* speed }
            in Map.insert id1 r1'' $ Map.insert id2 r2'' m
          _ -> m
  in
  Map.elems finalMap

-- NUEVA FUNCIÓN: Separar robots y redirigir su movimiento al chocar con obstáculos
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

                -- ✅ Colisión AABB (rectángulos), no círculos
                overlapX = (wr/2 + wo/2) - abs (vx delta)
                overlapY = (hr/2 + ho/2) - abs (vy delta)

            in if overlapX > 0 && overlapY > 0 then
                 -- Elegimos el eje de menor penetración (qué lado empujar)
                 let empuje
                        | overlapX < overlapY =
                            V2 (signum (vx delta) * overlapX) 0
                        | otherwise =
                            V2 0 (signum (vy delta) * overlapY)

                     newPos = pR ^+^ empuje
                     nuevaVel = velocity robot ^* 0.65  -- efecto "resbalar"

                 in Map.insert rid (robot { position = newPos, velocity = nuevaVel }) m
               else m

          _ -> m

  in Map.elems finalMap

stepWorld :: Tiempo -> World -> World
stepWorld dt w@World{gs, robots, shots, explosions, tick, elapsed, estado, endTimer, ganador, obstaculos}
  | estado == FinJuego = w
  | otherwise =
      let
        vivos = filter RB.isRobotAlive robots
        humanosVivos = filter (\r -> tipo (extras r) == Humano) vivos
        zombiesVivos = filter (\r -> tipo (extras r) == Zombie) vivos

        (nuevoEstado, nuevoTimer, nuevoGanador) =
          if null humanosVivos then
            if endTimer + dt >= 2.5
            then (FinJuego, endTimer + dt, Just Zombie)
            else (Jugando, endTimer + dt, Just Zombie)
          else if null zombiesVivos then
            if endTimer + dt >= 2.5
            then (FinJuego, endTimer + dt, Just Humano)
            else (Jugando, endTimer + dt, Just Humano)
          else (Jugando, 0, Nothing)

      in
        if nuevoEstado == FinJuego then
          w { estado = FinJuego
            , ganador = nuevoGanador
            , shots = []
            , robots = robots
            , explosions = updateExplosions explosions
          }

        else
          let
            ------------------------------------------------------------------
            -- BOMBERMAN: Activación solo si lo toca un robot
            ------------------------------------------------------------------
            activarBombermanPorContacto :: [Obstaculo] -> [(Int, Int)] -> [Obstaculo]
            activarBombermanPorContacto obst colRO =
              [ case tipoObs (extras o) of
                  Explosivo ->
                    if any ((== objectId o) . snd) colRO && not (activado (extras o))
                    then o { extras = (extras o) { activado = True, tiempoVida = 2 }}
                    else o
                  _ -> o
              | o <- obst
              ]

            colRO = detectRobotObstaculoCollisions robots obstaculos
            obstaculosActivados = activarBombermanPorContacto obstaculos colRO

            ------------------------------------------------------------------
            -- Cuenta atrás + animación lenta con animTimer
            ------------------------------------------------------------------
            obstaculosActualizados =
              [ case tipoObs (extras o) of
                  Explosivo | activado (extras o) && not (exploto (extras o)) ->
                    let ex = extras o
                        tiempoNuevo     = tiempoVida ex - dt
                        animTimerNuevo  = animTimer ex + dt

                        (frameNuevo, timerFinal) =
                          if animTimerNuevo >= 0.12
                          then ((animFrame ex + 1) `mod` 4, animTimerNuevo - 0.12)
                          else (animFrame ex, animTimerNuevo)

                        exFinal = ex
                          { tiempoVida = tiempoNuevo
                          , animFrame  = frameNuevo
                          , animTimer  = timerFinal
                          }
                    in o { extras = exFinal }

                  _ -> o
              | o <- obstaculosActivados
              ]

            ------------------------------------------------------------------
            -- Si el tiempo llega a 0 → Explota
            ------------------------------------------------------------------
            (obstaculosExplosivos, obstaculosNormales) =
              partition
                (\o -> tipoObs (extras o) == Explosivo
                       && activado (extras o)
                       && not (exploto (extras o))
                       && tiempoVida (extras o) <= 0)
                obstaculosActualizados

            nuevasExplosionesObstaculos =
              [ Explosion (position o) 0 1.2 | o <- obstaculosExplosivos ]  -- Reducido de 3.5 a 1.2

            obstaculosFinales =
              [ if objectId o `elem` map objectId obstaculosExplosivos
                then o { extras = (extras o) { exploto = True } }
                else o
              | o <- obstaculosNormales
              ]

            ------------------------------------------------------------------
            -- Resto del juego (NO TOCAR)
            ------------------------------------------------------------------
            decisiones =
              [ (r, RB.botDecision tick gs obstaculosFinales r (filter ((/= objectId r) . objectId) vivos))
              | r <- vivos
              ]

            (robotsAccionados, nuevosDisparos) =
              unzip [ applyBotActions gs vivos r a | (r,a) <- decisiones ]

            robotsMovidos =
              map (clampRobot (worldSize gs) . (`RB.updatePosition` dt)) robotsAccionados

            disparosMovidos =
              [ p { position = position p ^+^ velocity p ^* dt }
              | p <- shots ++ concat nuevosDisparos
              ]

            (colRR, colRP) = checkCollisions robotsMovidos disparosMovidos
            colPO = detectProyectilObstaculoCollisions disparosMovidos obstaculosFinales

            robotsConDañoColision = foldl aplicarDañoColision robotsMovidos colRR
            aplicarDañoColision rs (rid1, rid2) =
              case (find ((== rid1) . objectId) rs, find ((== rid2) . objectId) rs) of
                (Just r1, Just r2) | RB.esEnemigo r1 r2 ->
                  [ if objectId r == rid1 || objectId r == rid2
                    then aplicarDañoConEscudo 8 r  -- Usar nueva función con escudo
                    else r | r <- rs ]
                _ -> rs

            --  Aplicar daño de obstáculos Dañino (pinchos)
            robotsConDañoObstaculos = foldl aplicarDañoObstaculo robotsConDañoColision colRO
            aplicarDañoObstaculo rs (rid, oid) =
              case (find ((== rid) . objectId) rs, find ((== oid) . objectId) obstaculosFinales) of
                (Just r, Just obs) | tipoObs (extras obs) == Dañino ->
                  [ if objectId robot == rid
                    then aplicarDañoConEscudo (dañoObs (extras obs) * dt) robot  -- Daño proporcional al tiempo
                    else robot | robot <- rs ]
                _ -> rs

            robotsSeparadosDeObstaculos =
              separarRobotsDeObstaculos colRO robotsConDañoObstaculos obstaculosFinales

            robotsSeparados =
              separarRobotsEnColision colRR robotsSeparadosDeObstaculos

            ------------------------------------------------------------------
            -- Explosión de obstáculo aplica daño radial
            ------------------------------------------------------------------
            robotsConExplosiones =
              foldl aplicarDañoExplosion robotsSeparados obstaculosExplosivos
            aplicarDañoExplosion rs obs =
              let radio = radioExplosion (extras obs)
                  dano  = dañoObs (extras obs)
              in [ let dist = distanceBetween (position r) (position obs)
                       factor = max 0 (1 - dist / radio)
                       danoFinal = dano * factor
                   in if dist <= radio && danoFinal > 0
                      then aplicarDañoConEscudo danoFinal r  -- Usar nueva función con escudo
                      else r
                 | r <- rs ]

            idsParados = concatMap (\(a,b) -> [a,b]) colRR
            robotsParados =
              [ if objectId r `elem` idsParados then RB.updateRobotVelocity r (pure 0) else r
              | r <- robotsConExplosiones
              ]

            nuevasExplosiones =
              [ Explosion (position p) 0 0.5  -- Reducido de 1.0 a 0.5
              | (_, pid) <- colRP
              , p <- disparosMovidos
              , objectId p == pid
              ]

            nuevasExplosionesBalasObstaculos =
              [ Explosion (position p) 0 0.5  -- Reducido de 1.0 a 0.5
              | (pid, _) <- colPO
              , p <- disparosMovidos
              , objectId p == pid
              ]

            robotsDañados =
              foldl aplicarDañoBala robotsParados colRP
            aplicarDañoBala rs (rid,pid) =
              case (find ((== pid) . objectId) disparosMovidos, find ((== rid) . objectId) rs) of
                (Just p, Just robot) ->
                  let shooterId = ownerId (extras p)
                  in case find ((== shooterId) . objectId) robots of
                       Just shooter | RB.esEnemigo robot shooter ->
                         [ if objectId r == rid
                           then aplicarDañoConEscudo (damage (extras p)) r  -- Usar nueva función con escudo
                           else r | r <- rs ]
                       _ -> rs
                _ -> rs

            shotsRestantes =
              [ p
              | p <- disparosMovidos
              , objectId p `notElem` map snd colRP
              , objectId p `notElem` map fst colPO
              , isInBounds (position p) (worldSize gs)
              ]

            robotsFinal =
              [ clampRobot (worldSize gs) r { explosionTime = explosionTime r + dt }
              | r <- robotsDañados
              , not (explosion r && explosionTime r > 1.5)
              ]
            
            --  Recargar escudos de robots vivos
            robotsConEscudoRecargado = recargarEscudos dt robotsFinal

          in
            w { robots     = robotsConEscudoRecargado
              , shots      = shotsRestantes
              , explosions = updateExplosions (explosions ++ nuevasExplosiones ++ nuevasExplosionesObstaculos ++ nuevasExplosionesBalasObstaculos)
              , obstaculos = obstaculosFinales
              , tick       = tick + 1
              , elapsed    = elapsed + dt
              , endTimer   = nuevoTimer
              , ganador    = nuevoGanador
            }

--  Dibujar obstáculos
drawObstaculo :: Size -> Obstaculo -> G.Picture
drawObstaculo ws obs =
  let (x, y) = worldToScreen ws (position obs)
      V2 w h = size obs
      ex = extras obs

      sprite =
        case tipoObs ex of
          Bloqueante -> spriteObsBloqueante
          Dañino     -> spriteDanino
          Explosivo  ->
            let frame = animFrame ex `mod` length spritesBomber
            in spritesBomber !! frame

  in G.Translate x y $
       G.Scale (w / 1024) (h / 1024) sprite


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

    sizeRobot Humano = V2 35 28  -- Reducido de 45x35 a 35x28
    sizeRobot Zombie = V2 45 35  -- Reducido de 60x45 a 45x35

    energiaInicial Humano = 300  -- Aumentado de 200 a 300 para mayor durabilidad
    energiaInicial Zombie = 350  -- Aumentado de 250 a 350 para mayor durabilidad

    rangoVision Humano = 400
    rangoVision Zombie = 350

    velocidadMovimiento Humano = 65
    velocidadMovimiento Zombie = 75

--  Generar obstáculos aleatorios

-- Generar obstáculos aleatorios
generarObstaculos :: Int -> Float -> Float -> [Obstaculo]
generarObstaculos seed worldW worldH =
  let
      margen = 140
      totalBloqueantes = 4  -- Reducido de 8 a 4
      totalDaninos     = 3  -- Reducido de 6 a 3
      totalExplosivos  = 6  -- Reducido de 12 a 6

      esc1 = 0.08
      esc2=0.15

      tam Bloqueante =
        let (sw,sh) = tamSpriteBloqueante
        in V2 (sw * 0.10) (sh * 0.10)  -- Reducido de 0.20 a 0.10 (50% más pequeños)

      tam Dañino =
        let (sw,sh) = tamSpriteDanino
        in V2 (sw * esc1) (sh * esc1)

      tam Explosivo =
        let (sw,sh) = tamSpriteExplosivo
        in V2 (sw * esc2) (sh * esc2)

      -- Random determinista (no usa IO)
      rand :: Int -> Float
      rand n = fromIntegral ((1103515245 * (seed+n) + 12345) `mod` 2147483647) / 2147483647

      -- Chequeo de solapamiento (bounding box)
      seSolapa o1 o2 =
        let V2 w1 h1 = size o1
            V2 w2 h2 = size o2
            V2 x1 y1 = position o1
            V2 x2 y2 = position o2
        in abs (x1-x2) < (w1+w2)/2 && abs (y1-y2) < (h1+h2)/2

      -- Crea un obstáculo
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
                tipo        -- tipoObs
                60          -- dañoObs - Reducido de 100 a 60 para mayor durabilidad
                350         -- radioExplosion - Reducido de 400 a 350 para explosiones más pequeñas
                3           -- tiempoVida (cuenta atrás)
                False       -- exploto
                False       -- activado
                0           -- animFrame
                0           -- animTimer   
          }

      -- Intenta poner un obstáculo sin solaparse
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
