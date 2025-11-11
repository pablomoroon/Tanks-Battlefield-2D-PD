{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Main where

import Entidades
import Fisicas hiding (sub)
import Robot as RB
import Colisiones
import Assets (obtenerMapa, escalarMapaAlFondo, spritesBomber)
import Render (drawRobot, drawExplosion, drawImpactExplosion, drawBullet, drawHUDZombies
              ,drawSangreZombie, drawFinJuegoZombies, worldToScreen,drawImpactExplosion,drawImpactVeneno,drawVeneno)
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Interface.Pure.Game as GG
import Data.List (find, foldl', partition)
import qualified Data.Map.Strict as Map

-- NUEVO: lógica compartida
import GameLogicCore
  ( mkRobot, mkProjectile, clampRobot, applyBotActions
  , updateExplosions, aplicarDañoConEscudo, recargarEscudos
  , separarRobotsEnColision, separarRobotsDeObstaculos
  , spawnBando, generarObstaculos, drawObstaculo
  )

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

--------------------------------------------------------------------------------
-- SIMULACIÓN
--------------------------------------------------------------------------------

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
                          esHostil = tipoOwner /= tipoObjetivo
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

            robotsDañoTrampa =
              foldl
                (\rs (rid, oid) ->
                  case (find ((== rid) . objectId) rs, find ((== oid) . objectId) obstaculosFinales) of
                    (Just r, Just obs) | tipoObs (extras obs) == Dañino ->
                        map (\ro -> if objectId ro == rid then aplicarDañoConEscudo (dañoObs (extras obs)) ro else ro) rs
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
                robotsDañadosBalas
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
