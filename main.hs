{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Main where

import Entidades
import Fisicas hiding (sub)
import Robot as RB
import Colisiones
import Assets (obtenerMapa, escalarMapaAlFondo,
               spriteObsBloqueante, spriteDanino, spritesBomber,tamSpriteExplosivo,
               tamSpriteDanino,tamSpriteBloqueante,spritesVeneno,spriteSangre)
import Render (drawRobot, drawExplosion, drawImpactExplosion, drawBullet, drawHUDZombies
              ,drawSangreZombie, drawFinJuegoZombies, worldToScreen,drawImpactExplosion,drawImpactVeneno,drawVeneno)

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
  , sangre     :: [Sangre] 
  , tick       :: Int
  , elapsed    :: Tiempo
  , estado     :: EstadoJuego
  , endTimer   :: Float
  , ganador    :: Maybe TipoRobot
  , mapaActual :: Int
  } deriving (Show, Eq)


data Sangre = Sangre
  { sangrePos   :: Position 
  , sangreTime  :: Float  
  , sangreScale :: Float      
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
      G.Pictures (fondo : dibObsDaninos ++ dibRobots ++ dibExplosiones ++ dibObsNoDaninos ++ [mensajeFinal])
  | otherwise =
      G.Pictures (fondo : dibObsDaninos ++ dibRobots ++ dibBalas ++ dibExplosiones ++ dibSangre ++ dibObsNoDaninos ++ [hud])
  where
    V2 wv hv = worldSize (gs w)
    fondo = escalarMapaAlFondo wv hv (obtenerMapa (mapaActual w))

    -- separar obstáculos
    obsDaninos   = filter (\o -> tipoObs (extras o) == Dañino)   (obstaculos w)
    obsNoDaninos = filter (\o -> tipoObs (extras o) /= Dañino)   (obstaculos w)

    drawO = drawObstaculo (worldSize (gs w)) (elapsed w)
    dibObsDaninos   = map drawO obsDaninos
    dibObsNoDaninos = map drawO obsNoDaninos

    dibRobots = map (drawRobot  (worldSize (gs w))) (robots w)
    dibBalas  = map (drawBullet (worldSize (gs w))) (shots w)

    dibExplosiones =
      map (drawExplosion (worldSize (gs w))) (robots w)
      ++ [ if expTipo e == "veneno"
             then drawImpactVeneno     (worldSize (gs w)) (expPos e) (expTime e) (expScale e)
             else drawImpactExplosion  (worldSize (gs w)) (expPos e) (expTime e) (expScale e)
         | e <- explosions w
         ]
    dibSangre =[ drawSangreZombie (worldSize (gs w)) (sangrePos s) (sangreTime s) (sangreScale s)| s <- sangre w]


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
    , sangre = [] 
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
      Stop ->
        (RB.updateRobotVelocity rob (pure 0), ds)


      Rotate th ->
        (rob { angulo = rad2deg th }, ds)

      RotateCannon th ->
        (rob { anguloCanon = rad2deg th }, ds)

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

            tipoBala =
              if tipo (extras rob) == Zombie then "veneno" else "bullet"

            dañoBala =
              if tipoBala == "veneno" then 25 else 40

            newShot = mkProjectile
                        (objectId rob * 100000 + length ds)
                        posCanon
                        velBala
                        dañoBala
                        (objectId rob)
                        tipoBala
                        (tipo (extras rob))  
        in (rob, ds ++ [newShot])

      _ ->
        (rob, ds)

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


-- Recargar escudos con el tiempo
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
                
                r1'' = if normVel1 > 3  
                      then r1' { velocity = vel1 ^* factorReduccion }
                      else r1' { velocity = vel1 ^* 0.1 }  
                      
                r2'' = if normVel2 > 3
                      then r2' { velocity = vel2 ^* factorReduccion }
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
                 let empuje
                        | overlapX < overlapY =
                            V2 (signum (vx delta) * overlapX) 0
                        | otherwise =
                            V2 0 (signum (vy delta) * overlapY)
                     
                     newPos = pR ^+^ empuje
       
                     nuevaVel = velocity robot ^* 0.2  
                     
                 in Map.insert rid (robot { position = newPos, velocity = nuevaVel }) m
               else m
          _ -> m
  in Map.elems finalMap


stepWorld :: Tiempo -> World -> World
stepWorld dt w@World{gs, robots, shots, explosions, obstaculos, sangre, tick, elapsed, estado, endTimer, ganador}
  | estado == FinJuego =
      w { explosions = updateExplosions explosions }
  | otherwise =
      let
        vivos        = filter RB.isRobotAlive robots
        humanosVivos = filter (\r -> tipo (extras r) == Humano) vivos
        zombiesVivos = filter (\r -> tipo (extras r) == Zombie) vivos

        (nuevoEstado, nuevoTimer, nuevoGanador)
          | null humanosVivos =
              if endTimer + dt >= 2.5 then (FinJuego, endTimer + dt, Just Zombie)
                                       else (Jugando,  endTimer + dt, Just Zombie)
          | null zombiesVivos =
              if endTimer + dt >= 2.5 then (FinJuego, endTimer + dt, Just Humano)
                                       else (Jugando,  endTimer + dt, Just Humano)
          | otherwise = (Jugando, 0, Nothing)
      in
        if nuevoEstado == FinJuego then
          w { estado = FinJuego, ganador = nuevoGanador }
        else
          let
            obstaculosColisionables = filter (\o -> tipoObs (extras o) /= Dañino) obstaculos
            colRO                   = detectRobotObstaculoCollisions robots obstaculosColisionables

            -- Contacto con trampas dañinas 
            colTrampas = detectRobotObstaculoCollisions robots
                           (filter (\o -> tipoObs (extras o) == Dañino) obstaculos)

            obstaculosActivados =
              [ if tipoObs (extras o) == Explosivo
                    && any ((== objectId o) . snd) colRO
                    && not (activado (extras o))
                  then o { extras = (extras o) { activado = True, tiempoVida = 2 } }
                  else o
              | o <- obstaculos
              ]

            obstaculosActualizados =
              [ case tipoObs (extras o) of
                  Explosivo | activado (extras o) && not (exploto (extras o)) ->
                    let ex = extras o
                        animTimer'         = animTimer ex + dt
                        (frame', timer')   =
                          if animTimer' >= 0.12
                            then ((animFrame ex + 1) `mod` length spritesBomber, animTimer' - 0.12)
                            else (animFrame ex, animTimer')
                    in o { extras = ex { animFrame = frame', animTimer = timer', tiempoVida = tiempoVida ex - dt } }
                  _ -> o
              | o <- obstaculosActivados
              ]

            (obstaculosExplosivos, obstaculosRestantes) =
              partition
                (\o -> tipoObs (extras o) == Explosivo
                    && activado (extras o)
                    && not (exploto (extras o))
                    && tiempoVida (extras o) <= 0)
                obstaculosActualizados

            nuevasExplosionesObstaculos =
              [ Explosion (position o) 0 1.2 "normal" | o <- obstaculosExplosivos ]

            robotsDañoExplosivo =
              foldl
                (\rs o ->
                  let radio  = radioExplosion (extras o)
                      daño   = dañoObs (extras o)
                      centro = position o
                  in map (\r -> if distanceBetween (position r) centro <= radio
                                  then aplicarDañoConEscudo daño r else r) rs
                )
                robots
                obstaculosExplosivos

            obstaculosFinales =
              [ if objectId o `elem` map objectId obstaculosExplosivos
                  then o { extras = (extras o) { exploto = True } }
                  else o
              | o <- obstaculosRestantes
              ]

            decisiones =
              [ (r, RB.botDecision tick gs obstaculosFinales r (filter ((/= objectId r) . objectId) vivos))
              | r <- filter RB.isRobotAlive robotsDañoExplosivo
              ]

            (robotsTrasAccion, nuevosDisparos) =
              unzip [ applyBotActions gs vivos r act | (r, act) <- decisiones ]

            robotsMovidos =
              map (clampRobot (worldSize gs) . (`RB.updatePosition` dt)) robotsTrasAccion

            disparosMovidos =
              [ p { position = position p ^+^ velocity p ^* dt }
              | p <- shots ++ concat nuevosDisparos
              ]

            (colRR, colRP) = checkCollisions robotsMovidos disparosMovidos

            -- Las balas NO colisionan con obstáculo dañino
            obstaculosBloqueaBalas =
              filter (\o -> tipoObs (extras o) /= Dañino) obstaculosFinales
            colPO =
              detectProyectilObstaculoCollisions disparosMovidos obstaculosBloqueaBalas


            (robotsDañadosBalas, sangreNuevas) =
              foldl
                (\(rs, sangs) (rid, pid) ->
                  case ( find ((== pid) . objectId) disparosMovidos
                       , find ((== rid) . objectId) rs
                       ) of
                    (Just p, Just objetivo) ->
                      -- Verificar si el proyectil es hostil usando el tipo del dueño guardado
                      let tipoOwner = ownerTipo (extras p)
                          tipoObjetivo = tipo (extras objetivo)
                          esHostil = tipoOwner /= tipoObjetivo  -- Solo es hostil si los tipos son diferentes
                      in if not esHostil
                         then (rs, sangs)
                         else
                           let objetivo' = aplicarDañoConEscudo (damage (extras p)) objetivo
                               sang = if tipo (extras objetivo) == Zombie
                                      then [Sangre (position objetivo) 0 1.0]
                                      else []
                           in ( map (\x -> if objectId x == rid then objetivo' else x) rs
                              , sangs ++ sang )
                    _ -> (rs, sangs)
                )
                (robotsMovidos, [])
                colRP
            ------------------------------------------------------------------


            robotsDañoTrampa =
              foldl
                (\rs (rid, oid) ->
                  case (find ((== rid) . objectId) rs, find ((== oid) . objectId) obstaculosFinales) of
                    (Just _, Just obs) | tipoObs (extras obs) == Dañino ->
                      map (\ro -> if objectId ro == rid
                                  then aplicarDañoConEscudo (dañoObs (extras obs)) ro
                                  else ro) rs
                    _ -> rs
                )
                robotsDañadosBalas
                colTrampas

            robotsDañoColision =
              foldl
                (\rs (id1, id2) ->
                  case (find ((== id1) . objectId) rs, find ((== id2) . objectId) rs) of
                    (Just r1, Just r2) ->
                      let tipo1 = tipo (extras r1)
                          tipo2 = tipo (extras r2)
                      in if tipo1 /= tipo2
                           then
                             let humano = if tipo1 == Humano then r1 else r2
                                 humanoActualizado = aplicarDañoConEscudo 2 humano
                             in map (\r -> if objectId r == objectId humano then humanoActualizado else r) rs
                           else rs
                    _ -> rs
                )
                robotsDañoTrampa
                colRR

            robotsSeparadosDeObstaculos =
              separarRobotsDeObstaculos colRO robotsDañoColision obstaculosColisionables

            robotsSeparados =
              separarRobotsEnColision colRR robotsSeparadosDeObstaculos

            robotsFinal =
              [ clampRobot (worldSize gs) (r { explosionTime = explosionTime r + dt })
              | r <- robotsSeparados
              , not (explosion r && explosionTime r > 1.5)
              ]

            robotsEscudo =
              recargarEscudos dt robotsFinal

            shotsRestantes =
              [ p
              | p <- disparosMovidos
              , objectId p `notElem` (map snd colRP ++ map fst colPO)
              , isInBounds (position p) (worldSize gs)
              ]

         
            sangreActualizada =
              [ s { sangreTime = sangreTime s + dt }
              | s <- sangre
              , sangreTime s < 0.6
              ]
            ------------------------------------------------------------------

          in
            w { robots     = robotsEscudo
              , shots      = shotsRestantes
              , obstaculos = obstaculosFinales
              , explosions = updateExplosions (explosions
                                              ++ nuevasExplosionesObstaculos)
              , sangre     = sangreActualizada ++ sangreNuevas
              , tick       = tick + 1
              , elapsed    = elapsed + dt
              , endTimer   = nuevoTimer
              , ganador    = nuevoGanador
              , estado     = nuevoEstado
              }

--  Dibujar obstáculos
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
        in  min sx sy                            -- uniforme

  in case tipoObs ex of
       Bloqueante ->
         G.Translate x y $ G.Scale (w / 1024) (h / 1024) spriteBloq

       Dañino ->
         
         drawVeneno ws (position obs) (t * 1.0) escalaDanino
         

       Explosivo ->
         G.Translate x y $ G.Scale (w / 1024) (h / 1024) spriteExpl


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
        in abs (x1-x2) < (w1+w2)/2 && abs (y1-y2) < (h1+h2)/2

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
                tipo                             -- tipoObs
                (case tipo of
                    Bloqueante -> 0              -- edificios NO hacen daño
                    Dañino     -> 1             -- trampa simple hace daño al tocarse
                    Explosivo  -> 100            -- bomberman hace daño al explotar
                )
                300          -- radioExplosion
                3            -- tiempoVida
                False        -- exploto
                False        -- activado
                0            -- animFrame
                0            -- animTimer
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
