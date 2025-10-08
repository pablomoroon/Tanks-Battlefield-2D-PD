import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

-- Definición del estado del juego
data Juego = Juego
  { jugadorX    :: Float
  , objetivoPos :: (Float, Float)
  , puntos      :: Int
  , generador   :: StdGen
  } deriving Show

-- Estado inicial del juego
inicializar :: StdGen -> Juego
inicializar gen = Juego 0 (100, 100) 0 gen

-- Dibuja el estado del juego
dibujarJuego :: Juego -> Picture
dibujarJuego j = pictures
  [ color blue  $ translate (jugadorX j) (-200) (rectangleSolid 50 20)
  , color red   $ uncurry translate (objetivoPos j) (circleSolid 15)
  , color black $ translate (-350) 250 $
      scale 0.15 0.15 (text ("Puntos: " ++ show (puntos j)))
  ]

-- Maneja las teclas de movimiento
manejarEventoJuego :: Event -> Juego -> Juego
manejarEventoJuego evento j = case evento of
  EventKey (SpecialKey KeyLeft)  Down _ _ -> j { jugadorX = jugadorX j - 20 }
  EventKey (SpecialKey KeyRight) Down _ _ -> j { jugadorX = jugadorX j + 20 }
  _ -> j

-- Actualiza el estado del juego cada frame
actualizarJuego :: Float -> Juego -> Juego
actualizarJuego dt j =
  if colision
    then j'
    else j { objetivoPos = (ox, oy - 100 * dt) }
  where
    (ox, oy) = objetivoPos j
    colision = abs (ox - jugadorX j) < 40 && oy < -180
    (nuevaX, gen') = randomR (-350, 350) (generador j)
    j' = j { objetivoPos = (nuevaX, 250)
           , puntos      = puntos j + 1
           , generador   = gen'
           }

-- Función principal
main :: IO ()
main = do
  gen <- getStdGen
  play
    (InWindow "Atrapar" (800, 600) (10, 10)) -- ventana
    white                                    -- color de fondo
    60                                       -- FPS
    (inicializar gen)                        -- estado inicial
    dibujarJuego                             -- función de dibujo
    manejarEventoJuego                       -- manejo de eventos
    actualizarJuego                          -- actualización por frame
