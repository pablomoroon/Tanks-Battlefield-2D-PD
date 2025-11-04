{-# LANGUAGE OverloadedStrings #-}
module Assets where

import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Data.Bitmap as GB (loadBMP)
import Graphics.Gloss.Juicy (loadJuicyPNG)
import System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------
-- FUNCIONES AUXILIARES
--------------------------------------------------------------------------------

-- Carga segura: si no existe el archivo, muestra un aviso y devuelve un rectángulo de color.
loadSprite :: FilePath -> G.Picture -> G.Picture
loadSprite path fallback = unsafePerformIO $ do
  maybeImg <- loadJuicyPNG path
  case maybeImg of
    Just img -> return img
    Nothing  -> do
      putStrLn $ "[ADVERTENCIA] No se pudo cargar: " ++ path ++ ", usando color de relleno."
      return fallback

--------------------------------------------------------------------------------
-- SPRITES DE ROBOTS / PERSONAJES
--------------------------------------------------------------------------------

{-# NOINLINE spriteCuerpo #-}
spriteCuerpo :: G.Picture
spriteCuerpo =
  loadSprite "Assets/camioneta.png"
             (G.Color (G.greyN 0.5) (G.rectangleSolid 128 64))

{-# NOINLINE spriteCanion #-}
spriteCanion :: G.Picture
spriteCanion =
  loadSprite "Assets/persona.png"
             (G.Color G.white (G.rectangleSolid 64 32))

{-# NOINLINE spriteZombie #-}
spriteZombie :: G.Picture
spriteZombie =
  loadSprite "Assets/zombie.png"
             (G.Color G.green (G.rectangleSolid 64 32))

--------------------------------------------------------------------------------
-- MAPAS
--------------------------------------------------------------------------------

{-# NOINLINE mapaBosque #-}
mapaBosque :: G.Picture
mapaBosque = unsafePerformIO $ do
  putStrLn "Cargando mapa: bosque.bmp"
  return $
    loadSprite "Assets/bosque.png"
               (G.Color (G.greyN 0.3) (G.rectangleSolid 200 200))

{-# NOINLINE mapaDesierto #-}
mapaDesierto :: G.Picture
mapaDesierto = unsafePerformIO $ do
  putStrLn "Cargando mapa: desert.png"
  return $
    loadSprite "Assets/desert.png"
               (G.Color (G.greyN 0.3) (G.rectangleSolid 200 200))

{-# NOINLINE mapaCiudad #-}
mapaCiudad :: G.Picture
mapaCiudad = unsafePerformIO $ do
  putStrLn "Cargando mapa: mapaCiudad.png"
  return $
    loadSprite "Assets/mapaCiudad.png"
               (G.Color (G.greyN 0.3) (G.rectangleSolid 200 200))

-- Tamaño REAL (en píxeles) de cada sprite de obstáculo
tamSpriteBloqueante :: (Float, Float)
tamSpriteBloqueante = (1024 , 1024 )

tamSpriteDanino :: (Float, Float)
tamSpriteDanino = (1024 , 1024)

tamSpriteExplosivo :: (Float, Float)
tamSpriteExplosivo = (580, 600)


obtenerMapa :: Int -> G.Picture
obtenerMapa i = case i `mod` 3 of
  0 -> mapaBosque
  1 -> mapaDesierto
  _ -> mapaCiudad

-- Escalar mapa para ocupar toda la pantalla
escalarMapaAlFondo :: Float -> Float -> G.Picture -> G.Picture
escalarMapaAlFondo w h img = G.Scale (w / iw) (h / ih) img
  where
    (iw, ih) = (1050, 1070)

--------------------------------------------------------------------------------
-- SPRITES DE OBSTÁCULOS
--------------------------------------------------------------------------------

{-# NOINLINE spriteObsBloqueante #-}
spriteObsBloqueante :: G.Picture
spriteObsBloqueante =
  loadSprite "Assets/obs_bloqueante.png"
             (G.Color (G.greyN 0.4) (G.rectangleSolid 256 256))

{-# NOINLINE spriteDanino #-}
spriteDanino :: G.Picture
spriteDanino = unsafePerformIO $ do
  maybeImg <- loadJuicyPNG "Assets/trampa_simple.png"
  case maybeImg of
    Just img -> return img
    Nothing  -> return $ G.Color (G.makeColorI 200 50 50 255) (G.rectangleSolid 100 100)

{-# NOINLINE spriteExplosivo #-}
spriteExplosivo :: G.Picture
spriteExplosivo = unsafePerformIO $ do
  maybeImg <- loadJuicyPNG "Assets/Bomberman.png"
  case maybeImg of
    Just img -> return img
    Nothing  -> return $ G.Color (G.makeColorI 255 150 0 255) (G.rectangleSolid 100 100)


{-# NOINLINE spritesExplosion #-}
spritesExplosion :: [G.Picture]
spritesExplosion = unsafePerformIO $ do
  let paths = [ "Assets/explosion1.png"
              , "Assets/explosion2.png"
              , "Assets/explosion3.png"
              , "Assets/explosion4.png"
              ]
  imgs <- mapM loadJuicyPNG paths
  return [img | Just img <- imgs]

{-# NOINLINE spritesBomber #-}
spritesBomber :: [G.Picture]
spritesBomber = unsafePerformIO $ do
  let paths =
        [ "Assets/bomber1.png"
        , "Assets/bomber2.png"
        , "Assets/bomber3.png"
        , "Assets/bomber4.png"
        ]
  imgs <- mapM loadJuicyPNG paths
  return [img | Just img <- imgs]
