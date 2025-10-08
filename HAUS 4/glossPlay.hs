import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Char (toLower)

-- El mundo ahora guarda (x, y)
type Mundo = (Float, Float)

main :: IO ()
main = play ventana fondo fps (0, 0) dibujar manejarEvento actualizar
  where
    ventana = InWindow "Control con Flechas, Teclas y Ratón" (800, 600) (100, 100)
    fondo   = white
    fps     = 60

-- Dibuja el cuadrado en su posición actual
dibujar :: Mundo -> Picture
dibujar (x, y) =
  Translate x y $
    color red (Polygon [(0,0), (100,0), (100,100), (0,100)])

-- Maneja teclas pulsadas y clic del ratón
manejarEvento :: Event -> Mundo -> Mundo
-- Flechas
manejarEvento (EventKey (SpecialKey c) Down _ _) (x, y)
  | c == KeyRight = (x + 10, y)
  | c == KeyLeft  = (x - 10, y)
  | c == KeyUp    = (x, y + 10)
  | c == KeyDown  = (x, y - 10)

-- Teclas WASD
manejarEvento (EventKey (Char c) Down _ _) (x, y)
  | toLower c == 'd' = (x + 10, y)
  | toLower c == 'a' = (x - 10, y)
  | toLower c == 'w' = (x, y + 10)
  | toLower c == 's' = (x, y - 10)

-- Clic del ratón → mover a esa posición
manejarEvento (EventKey (MouseButton LeftButton) Down _ (mx, my)) _ =
  (mx, my)

-- Cualquier otro evento no cambia nada
manejarEvento _ pos = pos

-- No cambia con el tiempo (solo con eventos)
actualizar :: Float -> Mundo -> Mundo
actualizar _ mundo = mundo
