--ALUMNOS (GRUPO 14):
  --DONATO JULIAN TRIGUEROS ACOSTA
  --JESÚS LOBATO FERNÁNDEZ
  --PABLO MORÓN GARCÍA


-- Definir los tipos básicos del juego y las funciones básicas de geometría.

-- 1. Tipos a definir:

--   - Point. Un punto 2D en el espacio.
--     Es mejor usar Float que Double ya que ocupa menos memoria y en un videojuego eso es vital.
--     Perderemos precisión pero no es tan importante.
type Point = (Float, Float)

--   - Vector. Vector siempre se considera que empieza en (0,0).
type Vector = (Float, Float)

--   - Angle. Un ángulo con decimales.
type Angle = Float

--   - Distance. Un valor de distancia con decimales.
type Distance = Float

--   - Position. Representa la posición de un objeto en un mundo 2D.
type Position = (Float, Float)


-- 2. Definir las siguientes funciones:

--   - distanceBetween :: Position -> Position -> Distance
--     Calcula la distancia euclidiana entre dos posiciones en el espacio.
--     Toma dos puntos como entrada y devuelve la distancia lineal que los separa.
distanceBetween :: Position -> Position -> Distance
distanceBetween (x1, y1) (x2, y2) =
  sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

--   - angleToTarget :: Position -> Position -> Angle
--     Determina el ángulo desde una posición origen hacia una posición objetivo.
--     Útil para calcular la dirección en la que debe apuntar o moverse un objeto.
angleToTarget :: Position -> Position -> Angle
angleToTarget (x1, y1) (x2, y2) =
  atan2 (y2 - y1) (x2 - x1)   -- Se usa atan2 ya que es una comparación de coordenadas

--   - deg2rad :: Angle -> Angle
--     Convierte un ángulo expresado en grados a su equivalente en radianes.
deg2rad :: Angle -> Angle
deg2rad x = x * pi / 180

--   - rad2deg :: Angle -> Angle
--     Convierte un ángulo expresado en radianes a su equivalente en grados.
rad2deg :: Angle -> Angle
rad2deg x = x * 180 / pi

--   - subVec :: Vector -> Vector -> Vector
--     Realiza la resta de dos vectores, devolviendo un nuevo vector que representa la diferencia entre ellos.
subVec :: Vector -> Vector -> Vector
subVec (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

--   - getVertices :: (Point, Point, Point, Point, Angle) -> [Point]
--     Genera una lista de vértices (puntos) a partir de cuatro puntos base y un ángulo de rotación.
getVertices :: (Point, Point, Point, Point, Angle) -> [Point]
getVertices ((x1, y1), (x2, y2), (x3, y3), (x4, y4), angulo) =
  let radianes = deg2rad angulo   -- El let es una definición local, solo sirve aquí
      rot (x, y) = (x * cos radianes - y * sin radianes,
                    x * sin radianes + y * cos radianes)
  in [rot (x1, y1), rot (x2, y2), rot (x3, y3), rot (x4, y4)]

--   - dot :: Point -> Point -> Float
--     Calcula el producto escalar (dot product) entre dos puntos tratados como vectores.
dot :: Point -> Point -> Float
dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

--   - sub :: Point -> Point -> Point
--     Resta un punto de otro, devolviendo un nuevo punto que representa la diferencia entre las coordenadas.
sub :: Point -> Point -> Point
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

--   - perp :: Vector -> Vector
--     Calcula el vector perpendicular a un punto dado (tratado como vector).
perp :: Vector -> Vector
perp (x, y) = (-y, x)

--   - isInBounds :: Point -> Size -> Bool
--     Verifica si un punto se encuentra dentro de los límites definidos por un tamaño dado.
type Size = (Float, Float)

isInBounds :: Point -> Size -> Bool
isInBounds (x, y) (width, height) =
  x >= 0 && x <= width && y >= 0 && y <= height

