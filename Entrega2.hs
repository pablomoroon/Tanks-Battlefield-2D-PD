import Data.Default

instance Default Robot where
    def = Robot { position=def, velocity=def, canonDirection = def, energy=def, range=def, explosion= False}

r1 :: Robot
r1 = Robot (0,0) (1,1) 5 100 50 False

r2 :: Robot
r2 = Robot (0,1) (2,1) 0 0 0 False
--1. Analiza el funcionamiento del juego y piensa en los tipos que son necesarios.
--   Realiza una lista durante el análisis visual y posteriormente implementa dichos TADs.
{-
    La clase Robot, donde tenga una posición, una velocidad, un rango y una energía
    Para ello tendremos que crear los tipos Position, ya hechos en la primera entrega, el tipo velocidad, el tipo rango y el tipo energía
-}

type Point = (Float, Float)

type Vector = (Float, Float)

type Angle = Float

type Distance = Float

type Tiempo = Float

type Position = (Float, Float)

type Velocity = (Float, Float)

type Size = (Float, Float)

type Scale = (Float, Float)

data Robot =
    Robot {
        imagenR :: String
        imagenC :: String
        position :: Position , 
        velocity :: Velocity ,
        canonDirection :: Angle ,
        energy :: Float ,
        range :: Float ,
        explosion :: Bool,
        size :: Size
        } 
        deriving (Show, Eq)

data Proyectil = 
    Proyectil{
      imagen :: String
      velocityP :: Velocity,
      damage :: Float,
      angulo :: Angle, 
      explosionP :: Bool
    }

data Action =
    Action{
      imagen :: String
      stop :: Bool,
      direction :: Vector,
      acelerate :: Float,
      existingTime :: Time,
      shoot :: Bool
    }

data Map = 
  Map {
    imagen :: String
    size :: Size
  }

--2. Refactoriza las funciones implementadas hasta ahora para usar:
--   - pattern matching
--   - listas por comprensión
--   - cláusulas where, if-then-else
--   - guardas o case-of cuando proceda

distanceBetween :: Position -> Position -> Distance
distanceBetween (x1, y1) (x2, y2) =
  sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

angleToTarget :: Position -> Position -> Angle
angleToTarget (x1, y1) (x2, y2) =
  atan2 (y2 - y1) (x2 - x1)   -- Se usa atan2 ya que es una comparación de coordenadas

deg2rad :: Angle -> Angle
deg2rad x = x * pi / 180

rad2deg :: Angle -> Angle
rad2deg x = x * 180 / pi

subVec :: Vector -> Vector -> Vector
subVec (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

getVertices :: (Point, Point, Point, Point, Angle) -> [Point]
getVertices ((x1, y1), (x2, y2), (x3, y3), (x4, y4), angulo) =
  let radianes = deg2rad angulo   -- El let es una definición local, solo sirve aquí
      rot (x, y) = (x * cos radianes - y * sin radianes,
                    x * sin radianes + y * cos radianes)
  in [rot (x1, y1), rot (x2, y2), rot (x3, y3), rot (x4, y4)]

dot :: Point -> Point -> Float
dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

sub :: Point -> Point -> Point
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

perp :: Vector -> Vector
perp (x, y) = (-y, x)

isInBounds :: Point -> Size -> Bool
isInBounds (x, y) (width, height) =
  x >= 0 && x <= width && y >= 0 && y <= height

--3. Implementa las siguientes funciones usando pattern matching con los TADs definidos anteriormente:

--   - detectedAgent: Determinar si un agente ha detectado a otro en caso de encontrarse dentro del rango de su radar
detectedAgent' :: Robot -> Robot -> Bool
detectedAgent' (Robot (x1,y1) _ _ _ r _) (Robot (x2,y2) _ _ _ _ _) =
    distanceBetween (x1,y1) (x2,y2) <= r


detectedAgent :: Robot -> Robot -> Bool
detectedAgent r1 r2 =
    distanceBetween (position r1) (position r2) <= range r1 

--   - isRobotAlive: True si la energía del robot es mayor a 0
isRobotAlive' :: Robot -> Bool
isRobotAlive' (Robot _ _ _ e _ _) = e > 0 

isRobotAlive :: Robot -> Bool
isRobotAlive r1 =
    (energy r1) > 0 

-- PREGUNTA A LA IA, COMO ACCEDO A UN VALOR EXACTO DE LA CLASE ROBOT


--   - countActiveRobots: Contar los robots que están vivos
countActiveRobots' :: [Robot] -> Integer
countActiveRobots' [] = 0
countActiveRobots' (robot:robots) 
    | isRobotAlive robot = 1 + countActiveRobots' robots
    | otherwise = countActiveRobots' robots 

countActiveRobots :: [Robot] -> Int
countActiveRobots rs = length [r | r <- rs, isRobotAlive r]
-- Una lista por comprensión: se queda con los robots cuya
-- energía sea positiva, y cuenta cuántos son.


--   - updateRobotVelocity: Actualiza la velocidad de un robot con una velocidad dada
-- PREGUNTA: COMO SE ACTUALIZA LA VELOCIDAD DE UN TIPO CON VARIOS CAMPOS EN HASKELL
updateRobotVelocity :: Robot -> Velocity -> Robot
updateRobotVelocity r v = r { velocity = v } 


--   - updateVelocity: Actualizar velocidad basada en la acción de movimiento
-- como accedo al valor de una dato de robot -> Me ha dado la idea de usar el let in o el fst y snd. He optado por el segundo
updateVelocity :: Action -> Robot -> Robot
updateVelocity (Action s (dx, dy) a shoot) r 
    | s == True = r { velocity = (0, 0)}
    | otherwise = r { velocity = (fst(velocity r) + a*dx, snd(velocity r) + a*dy)}

--   - updatePosition: Actualizar una posición en función de la velocidad y el incremento de tiempo

updatePosition :: Robot -> Tiempo -> Robot
updatePosition r@(Robot (x,y) (vx, vy) _ _ _ _) t = r { position = (x + vx * t, y + vy *t) }


updatePosition' :: Robot -> Tiempo -> Robot
updatePosition' r t =
  let (x, y)   = position r
      (vx, vy) = velocity r
  in r { position = (x + vx * t, y + vy * t) }

--r@Robot guarda robot en r pero saca los valores de la indicados, en este caso la posicion y la velocidad 


--   - mul: tal que (w,h) `mul` (sw,sh) = (w * sw, h * sh)
mul :: Size -> Scale -> Size
mul (w,h) (sw, sh) = (w * sw, h * sh)
