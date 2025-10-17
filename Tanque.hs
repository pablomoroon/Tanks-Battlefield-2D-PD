import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Char (toLower)
--Entidades
data Robot = Robot
  { rId    :: Int
  , rX     :: Float
  , rY     :: Float
  , rVida  :: Float
  } deriving (Show, Eq)

data Proyectil = Proyectil
  { pX :: Float
  , pY :: Float
  , pVX :: Float
  , pVY :: Float
  , pDamage :: Float
  , pOwner  :: Int
  } deriving (Show, Eq)

data Mundo = Mundo
  { robots      :: [Robot]
  , proyectiles :: [Proyectil]
  , explosiones :: [(Float, Float, Float)]  
  }

estadoInicial :: Mundo
estadoInicial = Mundo
  { robots =
      [ Robot 1 (-200) 0 100
      , Robot 2 (200)  0 100
      ]
  , proyectiles = []
  , explosiones = []
  }


main :: IO ()
main = play ventana fondo fps estadoInicial dibujar manejarEvento actualizar
  where
    ventana = InWindow "Tanques con colisiones" (800, 600) (100, 100)
    fondo   = white
    fps     = 60

--Dibujos 
dibujar :: Mundo -> Picture
dibujar m =
  Pictures $
    [ dibujaRobot r | r <- robots m ] ++
    [ proyectilPicture p | p <- proyectiles m ] ++
    [ Translate x y (explosionPicture t)
    | (x,y,t) <- explosiones m ]

dibujaRobot :: Robot -> Picture
dibujaRobot r =
  Pictures
    [ cuerpoTanque (rX r, rY r)
    , cabeza      (rX r+10, rY r)
    , cañon       (rX r-30, rY r)
    , barraVida   (rX r, rY r+80) (rVida r)
    ]

-- ----- Partes del tanque -----
cuerpoTanque :: (Float, Float) -> Picture
cuerpoTanque (x,y)= 
    Translate x y $ Pictures
      [ Color (greyN 0.1) (rectangleSolid (100) (75))
      , Color black (rectangleSolid (100+6) (50+6))
      , Color colorTanque (rectangleSolid 100 50)
      ]

cabeza :: (Float, Float)->Picture
cabeza (x,y) = 
    Translate x y $ Pictures
      [ Color black (circleSolid(15+2))
      , Color colorTanque (circleSolid 15)
      ]

cañon :: (Float, Float)->Picture
cañon (x,y) = 
    Translate x y $ Pictures 
      [ Color black (rectangleSolid (80+3) (5+3)) 
      , Color colorTanque (rectangleSolid 80 5)
      ]

proyectilPicture :: Proyectil -> Picture
proyectilPicture p =
  Translate (pX p) (pY p) $ Pictures 
    [ Color black (circleSolid 4) 
    , Color (greyN 0.5) (circleSolid 3)
    ]

barraVida :: (Float, Float) -> Float -> Picture
barraVida (x,y) v =
  Translate x y $ Pictures
    [ Color black (rectangleSolid (100+3) (5+3))
    , Translate (-(100 - v)/2) 0 $
        Color (vidaColor v) (rectangleSolid v 5)
    ]

vidaColor :: Float -> Color
vidaColor v
  | v > 60    = green
  | v > 30    = orange
  | otherwise = red

colorTanque :: Color
colorTanque = makeColorI 75 83 32 255

--explosion
explosionPicture :: Float -> Picture
explosionPicture t =
  Pictures
    [ Color (withAlpha (1 - t/1.5) yellow)
        (circleSolid (5 + 10*t))
    , Color (withAlpha (0.9 - t/1.5) orange)
        (circleSolid (10 + 15*t))
    , Color (withAlpha (0.7 - t/1.5) red)
        (ThickCircle (15 + 20*t) 4)
    ]

--Controles
manejarEvento :: Event -> Mundo -> Mundo
manejarEvento (EventKey (SpecialKey k) Down _ _) m =
  case k of
    -- Movimiento tanque 1
    KeyRight -> moverRobot 1 (\r -> r { rX = rX r + 10 }) m
    KeyLeft  -> moverRobot 1 (\r -> r { rX = rX r - 10 }) m
    KeyUp    -> moverRobot 1 (\r -> r { rY = rY r + 10 }) m
    KeyDown  -> moverRobot 1 (\r -> r { rY = rY r - 10 }) m

    -- Disparo tanque 1
    KeySpace ->
      case buscarRobot 1 (robots m) of
        Just r1 -> m { proyectiles = proyectiles m ++
                        [Proyectil (rX r1 -80) (rY r1) (-250) 0 20 (rId r1)] }
        Nothing -> m

    -- Disparo tanque 2
    KeyEnter ->
      case buscarRobot 2 (robots m) of
        Just r2 -> m { proyectiles = proyectiles m ++
                        [Proyectil (rX r2 -80) (rY r2) (-250) 0 20 (rId r2)] }
        Nothing -> m

    _ -> m

manejarEvento (EventKey (Char c) Down _ _) m =
  case toLower c of
    -- Movimiento tanque 2
    'd' -> moverRobot 2 (\r -> r { rX = rX r + 10 }) m
    'a' -> moverRobot 2 (\r -> r { rX = rX r - 10 }) m
    'w' -> moverRobot 2 (\r -> r { rY = rY r + 10 }) m
    's' -> moverRobot 2 (\r -> r { rY = rY r - 10 }) m
    _   -> m

manejarEvento _ m = m

actualizar :: Float -> Mundo -> Mundo
actualizar dt m =
  let -- mover proyectiles
      proys = [ p { pX = pX p + pVX p * dt
                  , pY = pY p + pVY p * dt }
              | p <- proyectiles m ]
      -- detectar colisiones simples (caja)
      (nRobots, nProys, nExps) = colisiones (robots m) proys
  in m { robots = nRobots
       , proyectiles = nProys
       , explosiones = [ (x,y,t+dt) | (x,y,t) <- explosiones m, t < 2 ]
                       ++ nExps
       }

--Colisiones basicas de colisiones
colisiones :: [Robot] -> [Proyectil] -> ([Robot], [Proyectil], [(Float,Float,Float)])
colisiones rs ps = foldl aplicar (rs, [], []) ps
  where
    aplicar (robotsAcc, proysAcc, exps) p =
      case filter (hit p) robotsAcc of
        [] -> (robotsAcc, proysAcc ++ [p], exps)
        (r:_) ->
          let r' = r { rVida = rVida r - pDamage p }
              rs' = r':filter (/= r) robotsAcc
              ex  = (rX r, rY r, 0)
          in (rs', proysAcc, ex:exps)

    hit p r =
      abs (pX p - rX r) < 50 && abs (pY p - rY r) < 30

--mover
moverRobot :: Int -> (Robot -> Robot) -> Mundo -> Mundo
moverRobot rid f m =
  m { robots = [ if rId r == rid then f r else r | r <- robots m ] }

buscarRobot :: Int -> [Robot] -> Maybe Robot
buscarRobot rid = foldr (\r acc -> if rId r == rid then Just r else acc) Nothing
