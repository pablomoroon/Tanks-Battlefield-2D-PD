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
import Render (drawRobot, drawExplosion, drawImpactExplosion,drawImpactVeneno, drawBullet, drawHUDZombies,
 drawFinJuegoZombies, worldToScreen,drawVeneno)

import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Interface.Pure.Game as GG
import Data.List (find, foldl', partition)
import qualified Data.Map.Strict as Map
import System.IO (writeFile, appendFile, hFlush, stdout)
import Text.Printf (printf)
import Control.Monad (when)
import System.Directory (doesFileExist)
import Data.Maybe (fromMaybe)
import System.IO.Unsafe (unsafePerformIO)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Random (randomRIO)

--------------------------------------------------------------------------------
-- CONFIGURACIÓN DEL TORNEO
--------------------------------------------------------------------------------

leerConfiguracion :: IO (ConfigTorneo, Int)
leerConfiguracion = do
  existe <- doesFileExist "config.txt"
  if not existe
    then putStrLn "[!] config.txt no encontrado. Usando valores por defecto." >> return (configPorDefecto, 3)
    else do
      lineas <- lines <$> readFile "config.txt"
      let (valoresInt, valoresFloat) = parseConfig lineas
          get k d = fromMaybe d . lookup k
          config = ConfigTorneo
            { numHumanosTorneo = get "humanos" 5 valoresInt
            , numZombiesTorneo = get "zombies" 5 valoresInt
            , mapaIndexTorneo = get "mapa" 0 valoresInt
            , areaTorneo = (fromIntegral $ get "ancho" 1280 valoresInt, fromIntegral $ get "alto" 780 valoresInt)
            , duracionMaxima = get "duracion" 120.0 valoresFloat
            }
      return (config, get "total_torneos" 3 valoresInt)
  where
    configPorDefecto = ConfigTorneo
      { numHumanosTorneo = 5
      , numZombiesTorneo = 5
      , mapaIndexTorneo = 0
      , areaTorneo = (1280, 780)
      , duracionMaxima = 120.0
      }

-- Función auxiliar compartida
isPrefixOf :: String -> String -> Bool
isPrefixOf _ [] = False
isPrefixOf [] _ = True
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- Parser simple unificado
parseConfig :: [String] -> ([(String, Int)], [(String, Float)])
parseConfig lineas = (parsearInt lineas, parsearFloat lineas)
  where
    parsearInt ls = [(k, read v :: Int) | (k, v) <- parsear ls, all (`elem` "0123456789-") v]
    parsearFloat ls = [(k, read v :: Float) | (k, v) <- parsear ls, k == "duracion", all (`elem` "0123456789.-") v]
    parsear ls = [(fst p, dropWhile (== ' ') $ tail (snd p)) | l <- ls, not (null l), not ("#" `isPrefixOf` l), let p = break (== '=') l, snd p /= ""]

-- Configuración de un torneo individual
data ConfigTorneo = ConfigTorneo
  { numHumanosTorneo :: Int
  , numZombiesTorneo :: Int
  , mapaIndexTorneo  :: Int
  , areaTorneo       :: (Float, Float) -- (ancho, alto)
  , duracionMaxima   :: Float          -- en segundos
  } deriving (Show, Eq)

-- Estado del sistema de torneos
data TorneoState = TorneoState
  { torneoActual     :: Int           -- Número del torneo actual (0-indexed)
  , totalTorneos     :: Int           -- Total de torneos a ejecutar
  , torneoEnCurso    :: Maybe World   -- El mundo del torneo actual
  , esperandoInicio  :: Bool          -- Flag para iniciar siguiente torneo
  , tiempoEspera     :: Float         -- Tiempo de espera entre torneos
  , configuracion    :: ConfigTorneo  -- Configuración del torneo
  , estadisticas     :: [EstadisticasTorneo]  -- Estadísticas de torneos completados
  , semillasAleatorias :: [Int]       -- Semillas únicas para cada torneo
  } deriving (Show, Eq)

-- Estadísticas de un torneo individual
data EstadisticasTorneo = EstadisticasTorneo
  { numeroTorneo           :: Int
  , ganadorTorneo          :: Maybe TipoRobot
  , duracionTorneo         :: Float
  , proyectilesHumanos     :: Int
  , proyectilesZombies     :: Int
  , impactosHumanos        :: Int
  , impactosZombies        :: Int
  , tiempoVidaHumanos      :: Float  -- Tiempo total de vida de todos los humanos
  , tiempoVidaZombies      :: Float  -- Tiempo total de vida de todos los zombies
  , robotsInicialesHumanos :: Int
  , robotsInicialesZombies :: Int
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- ESTADOS PRINCIPALES MODIFICADOS PARA TORNEOS
--------------------------------------------------------------------------------

data AppState
  = TorneoMode TorneoState
  | Menu MenuState
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
  | BtnTorneos  
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
  -- Nuevos campos para estadísticas
  , disparosRealizadosHumanos :: Int
  , disparosRealizadosZombies :: Int
  , impactosRegistradosHumanos :: Int
  , impactosRegistradosZombies :: Int
  , tiempoAcumuladoHumanos :: Float
  , tiempoAcumuladoZombies :: Float
  } deriving (Show, Eq)


--------------------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "=== SISTEMA DE TORNEOS AUTOMATIZADOS ==="
  putStrLn "Iniciando modo torneo..."
  putStrLn ""
  

  putStrLn "Cargando configuración desde config.txt..."
  (config, totalTorneos) <- leerConfiguracion
  
  putStrLn $ "[OK] Configuracion cargada:"
  putStrLn $ "  - Humanos por torneo: " ++ show (numHumanosTorneo config)
  putStrLn $ "  - Zombies por torneo: " ++ show (numZombiesTorneo config)
  putStrLn $ "  - Mapa: " ++ nombreMapa (mapaIndexTorneo config)
  putStrLn $ "  - Area: " ++ show (areaTorneo config)
  putStrLn $ "  - Duracion maxima: " ++ show (duracionMaxima config) ++ " segundos"
  putStrLn $ "  - Total de torneos: " ++ show totalTorneos
  putStrLn ""
  putStrLn "Iniciando torneos en 3 segundos..."
  putStrLn ""
  
  -- Generar semillas aleatorias únicas para cada torneo
  semillas <- mapM (\_ -> randomRIO (1, 999999)) [1..totalTorneos]
  
  let initialTorneo = TorneoState
        { torneoActual = 0
        , totalTorneos = totalTorneos
        , torneoEnCurso = Nothing
        , esperandoInicio = True
        , tiempoEspera = 3  -- Tres segundos antes de empezar
        , configuracion = config
        , estadisticas = []
        , semillasAleatorias = semillas
        }
  
  GG.play
    G.FullScreen
    (G.makeColorI 15 15 25 255)
    60
    (TorneoMode initialTorneo)
    drawApp
    handleAppEvent
    stepApp

--------------------------------------------------------------------------------
-- FUNCIONES DE CREACIÓN DE TORNEO
--------------------------------------------------------------------------------
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


  

crearMundoTorneo :: ConfigTorneo -> Int -> Int -> World
crearMundoTorneo ConfigTorneo{..} torneoNum seed =
  let
    (worldW, worldH) = areaTorneo
    totalTanques = numHumanosTorneo + numZombiesTorneo
    gs0 = GameState { worldSize = V2 worldW worldH, nTanques = totalTanques }
    seedTorneo = seed * 97 + mapaIndexTorneo * 17 + torneoNum * 1000
    humanos = spawnBando numHumanosTorneo gs0 seedTorneo Humano 0
    zombies = spawnBando numZombiesTorneo gs0 (seedTorneo + 999) Zombie numHumanosTorneo
    obstaculosGenerados = generarObstaculos seedTorneo worldW worldH
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
    , mapaActual = mapaIndexTorneo
    , disparosRealizadosHumanos = 0
    , disparosRealizadosZombies = 0
    , impactosRegistradosHumanos = 0
    , impactosRegistradosZombies = 0
    , tiempoAcumuladoHumanos = 0
    , tiempoAcumuladoZombies = 0
    }

-- Iniciar un nuevo torneo
iniciarNuevoTorneo :: TorneoState -> TorneoState
iniciarNuevoTorneo ts@TorneoState{..} =
  if torneoActual >= totalTorneos
  then ts { esperandoInicio = False, torneoEnCurso = Nothing }
  else
    let semilla = if torneoActual < length semillasAleatorias 
                  then semillasAleatorias !! torneoActual
                  else torneoActual * 12345  -- Fallback por si acaso
        nuevoMundo = crearMundoTorneo configuracion torneoActual semilla
    in ts { torneoEnCurso = Just nuevoMundo
          , esperandoInicio = False
          , tiempoEspera = 0
          }

-- Finalizar torneo y recopilar estadísticas
finalizarTorneoIO :: World -> TorneoState -> IO TorneoState
finalizarTorneoIO mundo ts@TorneoState{..} = do
  let config = configuracion
      stats = EstadisticasTorneo
        { numeroTorneo = torneoActual + 1
        , ganadorTorneo = ganador mundo
        , duracionTorneo = elapsed mundo
        , proyectilesHumanos = disparosRealizadosHumanos mundo
        , proyectilesZombies = disparosRealizadosZombies mundo
        , impactosHumanos = impactosRegistradosHumanos mundo
        , impactosZombies = impactosRegistradosZombies mundo
        , tiempoVidaHumanos = tiempoAcumuladoHumanos mundo
        , tiempoVidaZombies = tiempoAcumuladoZombies mundo
        , robotsInicialesHumanos = numHumanosTorneo config
        , robotsInicialesZombies = numZombiesTorneo config
        }
      nuevasEstadisticas = estadisticas ++ [stats]
  
  -- Guardar estadística del torneo individual
  guardarEstadisticaTorneo stats
  
  -- Si es el último torneo, guardar resumen
  when (torneoActual + 1 >= totalTorneos) $ do
    guardarResumenFinal nuevasEstadisticas config
  
  return $ ts { torneoActual = torneoActual + 1
              , torneoEnCurso = Nothing
              , esperandoInicio = True
              , tiempoEspera = 2.0
              , estadisticas = nuevasEstadisticas
              }

-- Guardar estadística de un torneo individual
guardarEstadisticaTorneo :: EstadisticasTorneo -> IO ()
guardarEstadisticaTorneo stats = do
  let ganadorStr = case ganadorTorneo stats of
        Just Humano -> "HUMANOS"
        Just Zombie -> "ZOMBIES"
        Nothing -> "EMPATE"
      
      precisH :: Float
      precisH = if proyectilesHumanos stats > 0
                then (fromIntegral (impactosHumanos stats) / fromIntegral (proyectilesHumanos stats)) * 100
                else 0.0
      
      precisZ :: Float
      precisZ = if proyectilesZombies stats > 0
                then (fromIntegral (impactosZombies stats) / fromIntegral (proyectilesZombies stats)) * 100
                else 0.0
      tiempoPromedioH :: Float
      tiempoPromedioH =
        if robotsInicialesHumanos stats > 0
        then tiempoVidaHumanos stats / fromIntegral (robotsInicialesHumanos stats)
        else 0.0

      porcentajeVidaH :: Float
      porcentajeVidaH =
        if duracionTorneo stats > 0 && robotsInicialesHumanos stats > 0
        then (tiempoPromedioH / duracionTorneo stats) * 100
        else 0.0

      
      tiempoPromedioZ :: Float
      tiempoPromedioZ =
        if robotsInicialesZombies stats > 0
        then tiempoVidaZombies stats / fromIntegral (robotsInicialesZombies stats)
        else 0.0

      porcentajeVidaZ :: Float
      porcentajeVidaZ =
        if duracionTorneo stats > 0 && robotsInicialesZombies stats > 0
        then (tiempoPromedioZ / duracionTorneo stats) * 100
        else 0.0

      
      contenido = unlines
        [ "========================================="
        , "TORNEO #" ++ show (numeroTorneo stats)
        , "========================================="
        , "Ganador: " ++ ganadorStr
        , "Duracion: " ++ printf "%.2f" (duracionTorneo stats) ++ " segundos"
        , ""
        , "--- HUMANOS ---"
        , "Robots iniciales: " ++ show (robotsInicialesHumanos stats)
        , "Proyectiles disparados: " ++ show (proyectilesHumanos stats)
        , "Impactos realizados: " ++ show (impactosHumanos stats)
        , "Precision: " ++ printf "%.2f" precisH ++ "%"
        , "Tiempo promedio de vida: " ++ printf "%.2f" tiempoPromedioH ++ " segundos"
        , "Porcentaje de tiempo con vida: " ++ printf "%.2f" porcentajeVidaH ++ "%"
        , ""
        , "--- ZOMBIES ---"
        , "Robots iniciales: " ++ show (robotsInicialesZombies stats)
        , "Proyectiles disparados: " ++ show (proyectilesZombies stats)
        , "Impactos realizados: " ++ show (impactosZombies stats)
        , "Precision: " ++ printf "%.2f" precisZ ++ "%"
        , "Tiempo total de vida: " ++ printf "%.2f" (tiempoVidaZombies stats) ++ " segundos"
        , "Porcentaje de tiempo con vida: " ++ printf "%.2f" porcentajeVidaZ ++ "%"
        , ""
        ]
  
  -- Crear o agregar al archivo estadisticas.txt
  if numeroTorneo stats == 1
    then writeFile "estadisticas.txt" contenido
    else appendFile "estadisticas.txt" contenido
  
  putStrLn $ "[OK] Torneo #" ++ show (numeroTorneo stats) ++ " completado - Ganador: " ++ ganadorStr

-- Guardar resumen final de todos los torneos
guardarResumenFinal :: [EstadisticasTorneo] -> ConfigTorneo -> IO ()
guardarResumenFinal stats config = do
  let victoriasHumanos = length $ filter (\s -> ganadorTorneo s == Just Humano) stats
      victoriasZombies = length $ filter (\s -> ganadorTorneo s == Just Zombie) stats
      empates = length $ filter (\s -> ganadorTorneo s == Nothing) stats
      
      porcVictH :: Float
      porcVictH = (fromIntegral victoriasHumanos / fromIntegral (length stats)) * 100
      
      porcVictZ :: Float
      porcVictZ = (fromIntegral victoriasZombies / fromIntegral (length stats)) * 100
      
      totalDisparosH = sum $ map proyectilesHumanos stats
      totalImpactosH = sum $ map impactosHumanos stats
      totalDisparosZ = sum $ map proyectilesZombies stats
      totalImpactosZ = sum $ map impactosZombies stats
      
      promDisparosH = if not (null stats) then fromIntegral totalDisparosH / fromIntegral (length stats) else 0.0 :: Float
      promImpactosH = if not (null stats) then fromIntegral totalImpactosH / fromIntegral (length stats) else 0.0 :: Float
      promDisparosZ = if not (null stats) then fromIntegral totalDisparosZ / fromIntegral (length stats) else 0.0 :: Float
      promImpactosZ = if not (null stats) then fromIntegral totalImpactosZ / fromIntegral (length stats) else 0.0 :: Float
      
      precisTotalH = if totalDisparosH > 0
                    then (fromIntegral totalImpactosH / fromIntegral totalDisparosH) * 100
                    else 0.0 :: Float
      
      precisTotalZ = if totalDisparosZ > 0
                    then (fromIntegral totalImpactosZ / fromIntegral totalDisparosZ) * 100
                    else 0.0 :: Float
      
      duraciones = map duracionTorneo stats
      promDuracion = if not (null duraciones) then sum duraciones / fromIntegral (length duraciones) else 0.0 :: Float
      maxDuracion = if not (null duraciones) then maximum duraciones else 0.0 :: Float
      minDuracion = if not (null duraciones) then minimum duraciones else 0.0 :: Float
      
      tiemposVidaH = map tiempoVidaHumanos stats
      promVidaH = if not (null tiemposVidaH) then sum tiemposVidaH / fromIntegral (length tiemposVidaH) else 0.0 :: Float
      
      tiemposVidaZ = map tiempoVidaZombies stats
      promVidaZ = if not (null tiemposVidaZ) then sum tiemposVidaZ / fromIntegral (length tiemposVidaZ) else 0.0 :: Float
      
      resumen = unlines
        [ ""
        , "========================================="
        , "RESUMEN FINAL - TODOS LOS TORNEOS"
        , "========================================="
        , "Total de torneos: " ++ show (length stats)
        , ""
        , "--- RESULTADOS GLOBALES ---"
        , "Victorias Humanos: " ++ show victoriasHumanos ++ " (" ++ printf "%.1f" porcVictH ++ "%)"
        , "Victorias Zombies: " ++ show victoriasZombies ++ " (" ++ printf "%.1f" porcVictZ ++ "%)"
        , "Empates: " ++ show empates
        , ""
        , "--- ESTADISTICAS AGREGADAS HUMANOS ---"
        , "Total proyectiles disparados: " ++ show totalDisparosH
        , "Total impactos: " ++ show totalImpactosH
        , "Precision global: " ++ printf "%.2f" precisTotalH ++ "%"
        , "Promedio disparos por torneo: " ++ printf "%.1f" promDisparosH
        , "Promedio impactos por torneo: " ++ printf "%.1f" promImpactosH
        , "Promedio tiempo de vida: " ++ printf "%.2f" promVidaH ++ " segundos"
        , ""
        , "--- ESTADISTICAS AGREGADAS ZOMBIES ---"
        , "Total proyectiles disparados: " ++ show totalDisparosZ
        , "Total impactos: " ++ show totalImpactosZ
        , "Precision global: " ++ printf "%.2f" precisTotalZ ++ "%"
        , "Promedio disparos por torneo: " ++ printf "%.1f" promDisparosZ
        , "Promedio impactos por torneo: " ++ printf "%.1f" promImpactosZ
        , "Promedio tiempo de vida: " ++ printf "%.2f" promVidaZ ++ " segundos"
        , ""
        , "--- DURACIONES ---"
        , "Duracion promedio: " ++ printf "%.2f" promDuracion ++ " segundos"
        , "Duracion maxima: " ++ printf "%.2f" maxDuracion ++ " segundos"
        , "Duracion minima: " ++ printf "%.2f" minDuracion ++ " segundos"
        , ""
        , "========================================="
        , "Configuracion utilizada:"
        , "Humanos por torneo: " ++ show (numHumanosTorneo config)
        , "Zombies por torneo: " ++ show (numZombiesTorneo config)
        , "Duracion maxima: " ++ show (duracionMaxima config) ++ " segundos"
        , "Area: " ++ show (areaTorneo config)
        , "========================================="
        ]
  
  appendFile "estadisticas.txt" resumen
  putStrLn ""
  putStrLn "========================================="
  putStrLn "[OK] TODOS LOS TORNEOS COMPLETADOS"
  putStrLn "========================================="
  putStrLn $ "Victorias Humanos: " ++ show victoriasHumanos
  putStrLn $ "Victorias Zombies: " ++ show victoriasZombies
  putStrLn $ "Estadisticas guardadas en: estadisticas.txt"
  putStrLn "========================================="

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
-- EVENTOS
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
handleAppEvent (GG.EventKey (GG.SpecialKey GG.KeyEsc) GG.Down _ _) (TorneoMode _) =
  Menu initialMenu
handleAppEvent _ s = s

handleButtonClick :: MenuOption -> MenuState -> AppState
handleButtonClick btn m =
  case btn of
    BtnHumanosMinus -> let newNum = max 1 (numHumanos m - 1)
                       in Menu m { numHumanos = newNum, numZombies = newNum }
    BtnHumanosPlus  -> let newNum = min 10 (numHumanos m + 1)
                       in Menu m { numHumanos = newNum, numZombies = newNum }
    BtnZombiesMinus -> let newNum = max 1 (numZombies m - 1)
                       in Menu m { numHumanos = newNum, numZombies = newNum }
    BtnZombiesPlus  -> let newNum = min 10 (numZombies m + 1)
                       in Menu m { numHumanos = newNum, numZombies = newNum }
    BtnMapa         -> Menu m { mapIndex = (mapIndex m + 1) `mod` 3 }
    BtnStart        -> Playing (crearMundoDesdeMenu m)
    BtnTorneos      -> TorneoMode (unsafePerformIO $ iniciarTorneoDesdeMenu m)

-- Crear estado de torneo desde menú
iniciarTorneoDesdeMenu :: MenuState -> IO TorneoState
iniciarTorneoDesdeMenu MenuState{..} = do
  -- Generar semillas aleatorias para 3 torneos
  semillas <- mapM (\_ -> randomRIO (1, 999999)) [1..3]
  
  let config = ConfigTorneo
        { numHumanosTorneo = numHumanos
        , numZombiesTorneo = numZombies
        , mapaIndexTorneo  = mapIndex
        , areaTorneo       = (1280, 780)
        , duracionMaxima   = 120.0
        }
      initialState = TorneoState
        { torneoActual = 0
        , totalTorneos = 3
        , torneoEnCurso = Nothing
        , esperandoInicio = True
        , tiempoEspera = 0
        , configuracion = config
        , estadisticas = []
        , semillasAleatorias = semillas
        }
  return $ iniciarNuevoTorneo initialState

detectarBoton :: (Float, Float) -> Maybe MenuOption
detectarBoton (x, y)
  | dentro (-270, 40) 70 50 = Just BtnHumanosMinus
  | dentro (-130, 40) 70 50 = Just BtnHumanosPlus
  | dentro (160, 40) 70 50 = Just BtnZombiesMinus
  | dentro (300, 40) 70 50 = Just BtnZombiesPlus
  | dentro (0, -40) 300 60 = Just BtnMapa
  | dentro (20, -130) 500 80 = Just BtnStart
  | dentro (20, -230) 500 80 = Just BtnTorneos  -- Nuevo botón
  | otherwise = Nothing
  where
    dentro (cx, cy) w h = abs (x - cx) <= w/2 && abs (y - cy) <= h/2

--------------------------------------------------------------------------------
-- DIBUJO
--------------------------------------------------------------------------------

drawApp :: AppState -> G.Picture
drawApp (Menu m)         = drawMenu m
drawApp (Playing w)      = drawWorld w
drawApp (TorneoMode ts)  = drawTorneoState ts

drawTorneoState :: TorneoState -> G.Picture
drawTorneoState TorneoState{..} =
  case torneoEnCurso of
    Just mundo -> 
      let worldPic = drawWorld mundo
          overlay = drawTorneoOverlay torneoActual totalTorneos (elapsed mundo) (duracionMaxima configuracion)
      in G.Pictures [worldPic, overlay]
    Nothing ->
      if torneoActual >= totalTorneos
      then drawResumenFinal estadisticas
      else drawEsperandoTorneo torneoActual totalTorneos tiempoEspera

-- Overlay con información del torneo actual
drawTorneoOverlay :: Int -> Int -> Float -> Float -> G.Picture
drawTorneoOverlay actual total tiempo durMax =
  let x = -600
      y = 350
      torneoText = "TORNEO " ++ show (actual + 1) ++ " / " ++ show total
      tiempoText = "TIEMPO: " ++ show (floor tiempo :: Int) ++ "s / " ++ show (floor durMax :: Int) ++ "s"
  in G.Pictures
    [ G.Translate x y $
        G.Scale 0.2 0.2 $
          G.Color (G.makeColorI 255 230 100 255) (G.Text torneoText)
    , G.Translate x (y - 30) $
        G.Scale 0.15 0.15 $
          G.Color G.white (G.Text tiempoText)
    ]

-- Pantalla de espera entre torneos
drawEsperandoTorneo :: Int -> Int -> Float -> G.Picture
drawEsperandoTorneo actual total espera =
  let mensaje = "PREPARANDO TORNEO " ++ show (actual + 1) ++ " / " ++ show total
      cuenta = "Iniciando en: " ++ show (ceiling espera :: Int)
  in G.Pictures
    [ G.Color (G.makeColorI 15 15 25 255) (G.rectangleSolid 1280 800)
    , G.Translate (-300) 0 $
        G.Scale 0.4 0.4 $
          G.Color (G.makeColorI 255 230 100 255) (G.Text mensaje)
    , G.Translate (-150) (-80) $
        G.Scale 0.3 0.3 $
          G.Color G.white (G.Text cuenta)
    ]

-- Resumen final de todos los torneos
drawResumenFinal :: [EstadisticasTorneo] -> G.Picture
drawResumenFinal stats =
  let titulo = G.Translate (-400) 350 $
        G.Scale 0.4 0.4 $
          G.Color (G.makeColorI 255 230 100 255) (G.Text "TORNEOS COMPLETADOS")
      
      totalHumanos = length $ filter (\s -> ganadorTorneo s == Just Humano) stats
      totalZombies = length $ filter (\s -> ganadorTorneo s == Just Zombie) stats
      
      resumenText = "Victorias Humanos: " ++ show totalHumanos ++ 
                    "  |  Victorias Zombies: " ++ show totalZombies
      
      resumen = G.Translate (-300) 250 $
        G.Scale 0.25 0.25 $
          G.Color G.white (G.Text resumenText)
      
      mensaje = G.Translate (-350) (-350) $
        G.Scale 0.2 0.2 $
          G.Color (G.greyN 0.7) (G.Text "Presiona ESC para volver al menu")
      
  in G.Pictures [titulo, resumen, mensaje]

drawWorld :: World -> G.Picture
drawWorld w
  | estado w == FinJuego =
      G.Pictures (fondo : dibObsDaninos ++ dibRobots ++ dibExplosiones ++ dibObsNoDaninos ++ [mensajeFinal])
  | otherwise =
      G.Pictures (fondo : dibObsDaninos ++ dibRobots ++ dibBalas ++ dibExplosiones ++ dibObsNoDaninos ++ [hud])
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

    humanosVivos = length $ filter (\r -> RB.isRobotAlive r && tipo (extras r) == Humano) (robots w)
    zombiesVivos = length $ filter (\r -> RB.isRobotAlive r && tipo (extras r) == Zombie) (robots w)

    hud = drawHUDZombies (worldSize (gs w)) (tick w) (elapsed w) humanosVivos zombiesVivos
    mensajeFinal = drawFinJuegoZombies (ganador w)

drawMenu :: MenuState -> G.Picture
drawMenu MenuState{..} =
  let
    panelW = 850
    panelH = 650  
    baseColor = G.makeColorI 25 35 55 255

    titulo =
      G.Translate (-330) 220 $
        G.Scale 0.4 0.4 $
          G.Color (G.makeColorI 255 230 100 255) (G.Text "HUMANOS vs ZOMBIES")

    sub =
      G.Translate (-120) 180 $
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
    , boton "MODO TORNEO" (20, -230) 500 80 (G.makeColorI 100 255 200 255)
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
stepApp dt (TorneoMode ts) = TorneoMode (stepTorneo dt ts)


stepTorneo :: Float -> TorneoState -> TorneoState
stepTorneo dt ts@TorneoState{..}
  | esperandoInicio =
      if tiempoEspera <= 0
      then iniciarNuevoTorneo ts
      else ts { tiempoEspera = tiempoEspera - dt }
  | otherwise =
      case torneoEnCurso of
        Nothing -> ts
        Just mundo ->
          let mundoActualizado = stepWorld dt mundo
              duracionExcedida = elapsed mundoActualizado >= duracionMaxima configuracion
              torneoTerminado = estado mundoActualizado == FinJuego || duracionExcedida
          in if torneoTerminado
             then unsafePerformIO $ finalizarTorneoIO mundoActualizado ts
             else ts { torneoEnCurso = Just mundoActualizado }

--------------------------------------------------------------------------------
-- CREAR MUNDO DESDE EL MENÚ (para modo normal)
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
    , disparosRealizadosHumanos = 0
    , disparosRealizadosZombies = 0
    , impactosRegistradosHumanos = 0
    , impactosRegistradosZombies = 0
    , tiempoAcumuladoHumanos = 0
    , tiempoAcumuladoZombies = 0
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
aplicarDañoConEscudo dano r =
  let ex = extras r
      (dEscudo, dVida) = (min (shield ex) dano, max 0 (dano - shield ex))
      muere = energy ex - dVida <= 0
  in r { extras = ex { shield = max 0 (shield ex - dano), energy = energy ex - dVida
                     , shieldRechargeDelay = 2.0, damageFlash = 0.2 }
       , explosion = muere, explosionTime = if muere then 0 else explosionTime r }

recargarEscudos :: Float -> [Robot] -> [Robot]
recargarEscudos dt = map recargar
  where
    recargar r =
      let ex = extras r
          delay' = max 0 (shieldRechargeDelay ex - dt)
          escudo' = if delay' == 0 then min (maxShield ex) (shield ex + shieldRechargeRate ex * dt) else shield ex
          flash' = max 0 (damageFlash ex - dt)
      in r { extras = ex { shieldRechargeDelay = delay', shield = escudo', damageFlash = flash' } }

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

-- FUNCIÓN stepWorld con tracking de estadísticas
stepWorld :: Tiempo -> World -> World
stepWorld dt w@World{gs, robots, shots, explosions, obstaculos, tick, elapsed, estado, endTimer, ganador}
  | estado == FinJuego =
      w { explosions = updateExplosions explosions }

  | otherwise =
      let vivos        = filter RB.isRobotAlive robots
          humanosVivos = filter (\r -> tipo (extras r) == Humano) vivos
          zombiesVivos = filter (\r -> tipo (extras r) == Zombie) vivos

          (nuevoEstado, nuevoTimer, nuevoGanador)
            | null humanosVivos =
                if endTimer + dt >= 2.5 then (FinJuego, endTimer + dt, Just Zombie)
                else (Jugando, endTimer + dt, Just Zombie)

            | null zombiesVivos =
                if endTimer + dt >= 2.5 then (FinJuego, endTimer + dt, Just Humano)
                else (Jugando, endTimer + dt, Just Humano)

            | otherwise = (Jugando, 0, Nothing)

      in if nuevoEstado == FinJuego then
          w { estado = FinJuego, ganador = nuevoGanador }

      else
        let --------------------------------------------------------------------
            -- MANEJO DE OBSTÁCULOS (Activación, animación, explosión)
            --------------------------------------------------------------------
            obstaculosColisionables = filter (\o -> tipoObs (extras o) /= Dañino) obstaculos
            colRO = detectRobotObstaculoCollisions robots obstaculosColisionables
            colTrampas = detectRobotObstaculoCollisions robots (filter (\o -> tipoObs (extras o) == Dañino) obstaculos)

            obstaculosActivados =
              [ if tipoObs (extras o) == Explosivo
                   && any ((== objectId o) . snd) colRO
                   && not (activado (extras o))
                then o { extras = (extras o) { activado = True, tiempoVida = 2 }}
                else o
              | o <- obstaculos
              ]

            obstaculosActualizados =
              [ case tipoObs (extras o) of
                  Explosivo | activado (extras o) && not (exploto (extras o)) ->
                    let ex = extras o
                        animTimer' = animTimer ex + dt
                        (frame', timer') =
                          if animTimer' >= 0.12
                          then ((animFrame ex + 1) `mod` length spritesBomber, animTimer' - 0.12)
                          else (animFrame ex, animTimer')
                    in o { extras = ex { animFrame = frame', animTimer = timer', tiempoVida = tiempoVida ex - dt } }

                  Dañino ->
                    let ex = extras o
                        animTimer' = animTimer ex + dt
                        (frame', timerFinal) =
                          if animTimer' >= 0.20
                          then ((animFrame ex + 1) `mod` length spriteDanino, animTimer' - 0.20)
                          else (animFrame ex, animTimer')
                    in o { extras = ex { animFrame = frame', animTimer = timerFinal } }

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

            obstaculosFinales =
              [ if objectId o `elem` map objectId obstaculosExplosivos
                  then o { extras = (extras o) { exploto = True }}
                  else o
              | o <- obstaculosRestantes
              ]

            --------------------------------------------------------------------
            -- IA + DISPAROS + MOVIMIENTO
            --------------------------------------------------------------------
            decisiones =
              [ (r, RB.botDecision tick gs obstaculosFinales r (filter ((/= objectId r) . objectId) vivos))
              | r <- vivos ]

            (robotsAccionados, nuevosDisparos) =
              unzip [ applyBotActions gs vivos r acts | (r, acts) <- decisiones ]

            perShooter = [ (tipo (extras r), length ds) | (r, ds) <- zip robotsAccionados nuevosDisparos ]
            disparosHumanos = sum [ n | (Humano, n) <- perShooter ]
            disparosZombies = sum [ n | (Zombie, n) <- perShooter ]

            robotsMovidos =
              map (clampRobot (worldSize gs) . (`RB.updatePosition` dt)) robotsAccionados

            disparosMovidos =
              [ p { position = position p ^+^ velocity p ^* dt }
              | p <- shots ++ concat nuevosDisparos ]

            --------------------------------------------------------------------
            -- COLISIONES (robots, balas, trampas, explosivos)
            --------------------------------------------------------------------
            (colRR, colRP) = checkCollisions robotsMovidos disparosMovidos
            colPO = detectProyectilObstaculoCollisions disparosMovidos obstaculosFinales

            nuevasExplosionesBalas =
              [ Explosion (position p) 0 0.55
                  (if imagenObjeto p == "veneno" then "veneno" else "normal")
              | (_, pid) <- colRP ++ colPO
              , p <- disparosMovidos
              , objectId p == pid
              ]

            impactosHumanosNuevos =
              length [ () | (rid, _) <- colRP
                          , Just victima <- [find ((== rid) . objectId) robotsMovidos]
                          , tipo (extras victima) == Zombie ]

            impactosZombiesNuevos =
              length [ () | (rid, _) <- colRP
                          , Just victima <- [find ((== rid) . objectId) robotsMovidos]
                          , tipo (extras victima) == Humano ]

            robotsDañadosBalas =
              foldl
                (\rs (rid, pid) ->
                  case (find ((== pid) . objectId) disparosMovidos, find ((== rid) . objectId) rs) of
                    (Just p, Just objetivo) ->
                      -- Verificar si el proyectil es hostil usando el tipo del dueño guardado
                      let tipoOwner = ownerTipo (extras p)
                          tipoObjetivo = tipo (extras objetivo)
                          esHostil = tipoOwner /= tipoObjetivo  -- Solo es hostil si los tipos son diferentes
                      in if not esHostil
                         then rs
                         else map (\r -> if objectId r == rid then aplicarDañoConEscudo (damage (extras p)) r else r) rs
                    _ -> rs
                )
                robotsMovidos
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
                             -- Solo aplicar daño al humano cuando chocan un humano y un zombie
                             let humano = if tipo1 == Humano then r1 else r2
                                 humanoActualizado = aplicarDañoConEscudo 2 humano
                             in map (\r -> if objectId r == objectId humano then humanoActualizado else r) rs
                           else rs  -- No aplicar daño si son del mismo tipo (sin fuego amigo)
                    _ -> rs
                )
                robotsDañoTrampa
                colRR

            robotsSeparadosDeObstaculos =
              separarRobotsDeObstaculos colRO robotsDañoColision obstaculosColisionables

            robotsSeparados =
              separarRobotsEnColision colRR robotsSeparadosDeObstaculos

            robotsFinal =
              [ clampRobot (worldSize gs) r { explosionTime = explosionTime r + dt }
              | r <- robotsSeparados
              , not (explosion r && explosionTime r > 1.5)
              ]

            robotsEscudo = recargarEscudos dt robotsFinal

            shotsRestantes =
              [ p
              | p <- disparosMovidos
              , objectId p `notElem` map snd colRP ++ map fst colPO
              , isInBounds (position p) (worldSize gs)
              ]

        in w
            { robots = robotsEscudo
            , shots = shotsRestantes
            , obstaculos = obstaculosFinales
            , explosions = updateExplosions (explosions ++ nuevasExplosionesBalas ++ nuevasExplosionesObstaculos)
            , tick = tick + 1
            , elapsed = elapsed + dt
            , endTimer = nuevoTimer
            , ganador = nuevoGanador
            , estado = nuevoEstado

            , disparosRealizadosHumanos = disparosRealizadosHumanos w + disparosHumanos
            , disparosRealizadosZombies = disparosRealizadosZombies w + disparosZombies
            , impactosRegistradosHumanos = impactosRegistradosHumanos w + impactosHumanosNuevos
            , impactosRegistradosZombies = impactosRegistradosZombies w + impactosZombiesNuevos
            , tiempoAcumuladoHumanos = tiempoAcumuladoHumanos w + dt * fromIntegral (length humanosVivos)
            , tiempoAcumuladoZombies = tiempoAcumuladoZombies w + dt * fromIntegral (length zombiesVivos)
            }


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
       Bloqueante ->
         G.Translate x y $ G.Scale (w / 1024) (h / 1024) spriteBloq

       Dañino ->
         drawVeneno ws (position obs) (t * 1.0) escalaDanino  

       Explosivo ->
         G.Translate x y $ G.Scale (w / 1024) (h / 1024) spriteExpl

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
