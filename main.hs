{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Main where

import Entidades
import Fisicas
import Robot as RB
import Colisiones
import Assets (obtenerMapa, escalarMapaAlFondo)
import Render (drawRobot, drawExplosion, drawImpactExplosion, drawBullet, drawHUDZombies, drawFinJuegoZombies)

import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Interface.Pure.Game as GG
import Data.List (find, foldl')
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
  , tick       :: Int
  , elapsed    :: Tiempo
  , estado     :: EstadoJuego
  , endTimer   :: Float
  , ganador    :: Maybe TipoRobot
  , mapaActual :: Int
  } deriving (Show, Eq)

data Explosion = Explosion
  { expPos  :: Position
  , expTime :: Float
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------------------
main :: IO ()
main =
  GG.play
    (G.InWindow "Humanos vs Zombies" (1280, 800) (100, 100))
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

detectarBoton :: (Float, Float) -> Maybe MenuOption
detectarBoton (x, y)
  | dentro (-200, 60) 70 50 = Just BtnHumanosMinus
  | dentro (-90, 60)  70 50 = Just BtnHumanosPlus
  | dentro (90, 60)   70 50 = Just BtnZombiesMinus
  | dentro (200, 60)  70 50 = Just BtnZombiesPlus
  | dentro (0, -40)   300 60 = Just BtnMapa
  | dentro (0, -130)  360 80 = Just BtnStart
  | otherwise = Nothing
  where dentro (cx, cy) w h = abs (x - cx) <= w/2 && abs (y - cy) <= h/2

--------------------------------------------------------------------------------
-- DIBUJO DEL MENÚ CENTRADO
--------------------------------------------------------------------------------

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
  in
  World
    { gs = gs0
    , robots = humanos ++ zombies
    , shots = []
    , explosions = []
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
      , range  = rango
      , speed  = vel
      , tipo   = tipoR
      , memTarget = Nothing
      , memRole = Nothing
      , memLastSeen = Nothing
      , memAggroCooldown = 0
      }
    }

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
                12
                (objectId rob)
          in (rob, ds ++ [newShot])
        else (rob, ds)

      Combo xs -> foldl ejecutar (rob, ds) xs

updateExplosions :: [Explosion] -> [Explosion]
updateExplosions = filter ((<1.0) . expTime) . map (\e -> e { expTime = expTime e + 0.05 })

-- NUEVA FUNCIÓN: Separar robots que colisionan
separarRobotsEnColision :: [(Int, Int)] -> [Robot] -> [Robot]
separarRobotsEnColision colisiones robots =
  let robotMap = Map.fromList [(objectId r, r) | r <- robots]
      
      -- Aplicar todas las separaciones
      finalMap = foldl' aplicarSeparacion robotMap colisiones
      
      aplicarSeparacion m (id1, id2) =
        case (Map.lookup id1 m, Map.lookup id2 m) of
          (Just r1, Just r2) ->
            let (r1', r2') = RB.separarRobots r1 r2
            in Map.insert id1 r1' $ Map.insert id2 r2' m
          _ -> m
  in
  Map.elems finalMap

stepWorld :: Tiempo -> World -> World
stepWorld dt w@World{gs, robots, shots, explosions, tick, elapsed, estado, endTimer, ganador, mapaActual}
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
            , robots = robots
            , shots = []
            , explosions = updateExplosions explosions
            }
        else
          let
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

            dañoPorColisionZombie = 8
            robotsConDañoColision = foldl aplicarDañoColision robotsMovidos colRR
            aplicarDañoColision rs (rid1, rid2) =
              let r1 = find ((== rid1) . objectId) rs
                  r2 = find ((== rid2) . objectId) rs
              in case (r1, r2) of
                (Just robot1, Just robot2) ->
                  if RB.esEnemigo robot1 robot2 then
                    [ if objectId r == rid1 || objectId r == rid2
                      then let nuevaEnergia = energy (extras r) - dañoPorColisionZombie
                               muere = nuevaEnergia <= 0
                           in r { extras = (extras r) { energy = nuevaEnergia }
                                , explosion = explosion r || muere
                                , explosionTime = if muere then 0 else explosionTime r
                                }
                      else r
                    | r <- rs ]
                  else rs
                _ -> rs

            -- NUEVO: Separar físicamente los robots que colisionan
            robotsSeparados = separarRobotsEnColision colRR robotsConDañoColision

            idsParados = concatMap (\(a,b) -> [a,b]) colRR
            robotsParados =
              [ if objectId r `elem` idsParados then RB.updateRobotVelocity r (pure 0) else r
              | r <- robotsSeparados
              ]

            nuevasExplosiones =
              [ Explosion (position p) 0
              | (_, pid) <- colRP
              , p <- disparosMovidos
              , objectId p == pid
              ]

            robotsDañados = foldl aplicarDañoBala robotsParados colRP
            aplicarDañoBala rs (rid,pid) = case find ((== pid) . objectId) disparosMovidos of
              Nothing -> rs
              Just p  -> 
                case find ((== ownerId (extras p)) . objectId) robots of
                  Nothing -> rs
                  Just shooter ->
                    [ if objectId r == rid
                      then
                        if RB.esEnemigo r shooter
                        then let nuevaEnergia = energy (extras r) - damage (extras p)
                                 muere = nuevaEnergia <= 0
                             in r { extras = (extras r) { energy = nuevaEnergia }
                                  , explosion = explosion r || muere
                                  , explosionTime = if muere then 0 else explosionTime r
                                  }
                        else r
                      else r
                    | r <- rs ]

            shotsRestantes =
              [ p
              | p <- disparosMovidos
              , objectId p `notElem` map snd colRP
              , isInBounds (position p) (worldSize gs)
              ]

            robotsFinal =
              [ clampRobot (worldSize gs) r { explosionTime = explosionTime r + dt }
              | r <- robotsDañados
              , not (explosion r && explosionTime r > 1.5)
              ]
          in
            w { robots     = robotsFinal
              , shots      = shotsRestantes
              , explosions = updateExplosions (explosions ++ nuevasExplosiones)
              , tick       = tick + 1
              , elapsed    = elapsed + dt
              , endTimer   = nuevoTimer
              , ganador    = nuevoGanador
              }

drawWorld :: World -> G.Picture
drawWorld w
  | estado w == FinJuego =
      G.Pictures (fondo : dibRobots ++ dibExplosiones ++ [mensajeFinal])
  | otherwise =
      G.Pictures (fondo : dibRobots ++ dibBalas ++ dibExplosiones ++ [hud])
  where
    V2 wv hv = worldSize (gs w)
    fondo = escalarMapaAlFondo wv hv (obtenerMapa (mapaActual w))

    dibRobots = map (drawRobot (worldSize (gs w))) (robots w)
    dibBalas = map (drawBullet (worldSize (gs w))) (shots w)
    dibExplosiones =
      map (drawExplosion (worldSize (gs w))) (robots w)
      ++ [ drawImpactExplosion (worldSize (gs w)) (expPos e) (expTime e)
         | e <- explosions w
         ]

    humanosVivos = length $ filter (\r -> RB.isRobotAlive r && tipo (extras r) == Humano) (robots w)
    zombiesVivos = length $ filter (\r -> RB.isRobotAlive r && tipo (extras r) == Zombie) (robots w)

    hud = drawHUDZombies (worldSize (gs w)) (tick w) (elapsed w) humanosVivos zombiesVivos

    mensajeFinal = drawFinJuegoZombies (ganador w)

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
    
    sizeRobot Humano = V2 45 35
    sizeRobot Zombie = V2 60 45
    
    energiaInicial Humano = 200
    energiaInicial Zombie = 250
    
    rangoVision Humano = 400
    rangoVision Zombie = 350
    
    velocidadMovimiento Humano = 65
    velocidadMovimiento Zombie = 75
