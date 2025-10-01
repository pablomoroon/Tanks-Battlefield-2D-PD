import Data.Default

--ALUMNOS (GRUPO 14):
  --DONATO JULIAN TRIGUEROS ACOSTA
  --JESÚS LOBATO FERNÁNDEZ
  --PABLO MORÓN GARCÍA

--1. Analiza el funcionamiento del juego y piensa en los tipos que son necesarios.
--   Realiza una lista durante el análisis visual y posteriormente implementa dichos TADs.

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
        imagenR :: String,
        imagenC :: String,
        position :: Position , 
        velocity :: Velocity ,
        canonDirection :: Angle ,
        energy :: Float ,
        range :: Float ,
        explosion :: Bool,
        sizeR :: Size
        } 
        deriving (Show, Eq)

data Proyectil = 
    Proyectil{
      imagenP :: String,
      velocityP :: Velocity,
      damage :: Float,
      angulo :: Angle, 
      existingTime :: Tiempo,
      explosionP :: Bool
    }
    deriving (Show, Eq)

data Action =
    Action{
      stop :: Bool,
      direction :: Vector,
      acelerate :: Float,
      shoot :: Bool
    }
    deriving (Show, Eq)

data GameState = 
  GameState {
    imagenM :: String,
    size :: Size,
    nProyectiles :: Integer,
    nTanques :: Integer
  }
  deriving (Show, Eq)

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
detectedAgent :: Robot -> Robot -> Bool
detectedAgent r1 r2 =
    distanceBetween (position r1) (position r2) <= range r1 

--   - isRobotAlive: True si la energía del robot es mayor a 0
isRobotAlive :: Robot -> Bool
isRobotAlive r1 =
    (energy r1) > 0 

--   - countActiveRobots: Contar los robots que están vivos
countActiveRobots :: [Robot] -> Int
countActiveRobots rs = length [r | r <- rs, isRobotAlive r]
-- Una lista por comprensión: se queda con los robots cuya
-- energía sea positiva, y cuenta cuántos son.


--   - updateRobotVelocity: Actualiza la velocidad de un robot con una velocidad dada
updateRobotVelocity :: Robot -> Velocity -> Robot
updateRobotVelocity r v = r { velocity = v } 


--   - updateVelocity: Actualizar velocidad basada en la acción de movimiento
-- PREGUNTA A LA IA -> COMO MODIFICAR EL VALOR DE CADA UNO DE LOS VALORES DE UNA TUPLA
updateVelocity :: Action -> Robot -> Robot
updateVelocity (Action stop (dx, dy) a time shoot) r 
    | stop == True = r { velocity = (0, 0)}
    | otherwise = r { velocity = (fst(velocity r) + a*dx, snd(velocity r) + a*dy)}

--   - updatePosition: Actualizar una posición en función de la velocidad y el incremento de tiempo
updatePosition :: Robot -> Tiempo -> Robot
updatePosition r t =
  let (x, y)   = position r
      (vx, vy) = velocity r
  in r { position = (x + vx * t, y + vy * t) }


--   - mul: tal que (w,h) `mul` (sw,sh) = (w * sw, h * sh)
mul :: Size -> Scale -> Size
mul (w,h) (sw, sh) = (w * sw, h * sh)


-- HEMOS USADO LA IA PARA QUE NOS CORRIJA EL CÓDIGO UNA VEZ TERMINADO Y NOS CREE DOS ROBOTS PARA PODER HACER PRUEBAS

robot1 :: Robot
robot1 = Robot
    { imagenR        = "robot1.png"
    , imagenC        = "canon1.png"
    , position       = (0, 0)
    , velocity       = (1, 1)
    , canonDirection = 0
    , energy         = 100
    , range          = 50
    , explosion      = False
    , sizeR          = (10, 10)
    }

robot2 :: Robot
robot2 = Robot
    { imagenR        = "robot2.png"
    , imagenC        = "canon2.png"
    , position       = (30, 40)
    , velocity       = (0, 0)
    , canonDirection = 90
    , energy         = 0
    , range          = 50
    , explosion      = False
    , sizeR          = (10, 10)
    }


