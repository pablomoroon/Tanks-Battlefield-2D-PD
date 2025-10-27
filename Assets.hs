module Assets
  ( obtenerMapa
  , escalarMapaAlFondo
  , spriteCuerpo
  , spriteCanion
  , spriteZombie
  ) where


import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Data.Bitmap as GB (loadBMP)
import System.IO.Unsafe (unsafePerformIO)
import qualified Graphics.Gloss as G
import Graphics.Gloss.Juicy (loadJuicyPNG)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE spriteCuerpo #-}
spriteCuerpo :: G.Picture
spriteCuerpo = unsafePerformIO $ do
  Just img <- loadJuicyPNG "Assets/camioneta.png"
  return img

{-# NOINLINE spriteCanion #-}
spriteCanion :: G.Picture
spriteCanion = unsafePerformIO $ do
  Just img <- loadJuicyPNG "Assets/persona.png"
  return img

{-# NOINLINE spriteZombie #-}
spriteZombie :: G.Picture
spriteZombie = unsafePerformIO $ do
  Just img <- loadJuicyPNG "Assets/zombie.png"
  return img

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

-- MODIFICADO: Escalar mapa para ocupar toda la pantalla (sin márgenes)
escalarMapaAlFondo :: Float -> Float -> G.Picture -> G.Picture
escalarMapaAlFondo w h img =
  G.Scale (w / iw) (h / ih) img  -- Sin el 0.95, ocupa todo
  where
    (iw, ih) = (1050, 1070)