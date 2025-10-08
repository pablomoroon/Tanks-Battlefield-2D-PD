{--
import Graphics.Gloss

main :: IO ()
main = display ventana fondo imagen
  where
    ventana = InWindow "Mi Primera Ventana" (800, 600) (100, 100)
    fondo   = white
    imagen  = Circle 80
    
--}

{--
import Graphics.Gloss

main :: IO ()
main = animate ventana fondo imagen
  where
    ventana = InWindow "Mi Primera Ventana" (800, 600) (100, 100)
    fondo   = miColor
    imagen t = Translate (100 * cos t) (100 * sin t) (Circle 80)

miColor = makeColorI 71 66 59 255 -- RGBA (ROJO, VERDE, AZUL, ALPHA(TRANSPARENCIA))

--}

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

type Mundo = Float

main :: IO ()
main = play ventana fondo fps 0 dibujar manejarEvento actualizar
  where
    ventana = InWindow "Control con Flechas" (800, 600) (100, 100)
    fondo   = white
    fps     = 60

    dibujar :: Mundo -> Picture
    dibujar x = Translate x 0 (Circle 80)

    manejarEvento :: Event -> Mundo -> Mundo
    manejarEvento (EventKey (SpecialKey KeyRight) Down _ _) x = x + 10
    manejarEvento (EventKey (SpecialKey KeyLeft)  Down _ _) x = x - 10
    manejarEvento _ x = x

    actualizar :: Float -> Mundo -> Mundo
    actualizar _ x = x
