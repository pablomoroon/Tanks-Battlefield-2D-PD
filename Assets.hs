module Assets (obtenerMapa, escalarMapaAlFondo) where

import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Data.Bitmap as GB (loadBMP)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE mapaBosque #-}
mapaBosque :: G.Picture
mapaBosque = unsafePerformIO $ GB.loadBMP "mapas/bosque.bmp"

{-# NOINLINE mapaDesierto #-}
mapaDesierto :: G.Picture
mapaDesierto = unsafePerformIO $ GB.loadBMP "mapas/desierto.bmp"

{-# NOINLINE mapaCiudad #-}
mapaCiudad :: G.Picture
mapaCiudad = unsafePerformIO $ GB.loadBMP "mapas/ciudad.bmp"

obtenerMapa :: Int -> G.Picture
obtenerMapa i = case i `mod` 3 of
  0 -> mapaBosque
  1 -> mapaDesierto
  _ -> mapaCiudad

escalarMapaAlFondo :: Float -> Float -> G.Picture -> G.Picture
escalarMapaAlFondo w h pic =
  let s = max (w/1024) (h/1024)
  in G.Scale s s pic
