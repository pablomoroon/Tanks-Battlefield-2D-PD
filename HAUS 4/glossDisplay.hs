import Graphics.Gloss

main :: IO ()
main = display ventana fondo imagen
  where
    ventana = InWindow "Display" (800, 600) (100, 100)
    fondo   = white
    imagen  = figuras 



figuras = Pictures [Translate (-50) 0 $ color blue (circleSolid 100), Translate (75) 0 $ color red (circleSolid 20)]

    
