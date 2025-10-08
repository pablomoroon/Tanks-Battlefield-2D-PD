import Graphics.Gloss

main :: IO ()
main = animate ventana fondo imagen
  where
    ventana = InWindow "Pentágono animado" (800, 600) (100, 100)
    fondo   = white
    --imagen t = Translate (100 * cos t) (100 * sin t) (color red (pentagono 100))
    imagen t = Rotate (45*t) $
     Translate (100 * cos (2*t)) (100 * sin (2*t)) $
     Scale 1.5 1.5 $
     color miColor (pentagono 100) 


miColor :: Color
miColor = makeColorI 71 66 59 255   -- RGBA (R,V,B,A) — usa 255 en alpha para opaco

-- Genera un pentágono regular
pentagono :: Float -> Picture
pentagono r = Polygon
  [ (r * cos (2*pi*k/5), r * sin (2*pi*k/5))
  | k <- [0..4]
  ]
