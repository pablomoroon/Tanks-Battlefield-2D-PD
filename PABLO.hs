import Entidades
import Fisicas
import Robot


------------------------------------------------------------
-- Proyecta un polígono sobre un eje y devuelve el intervalo
-- (mínimo, máximo) de sus proyecciones. Se usa en el SAT.
------------------------------------------------------------
projectPolygon :: [Point] -> Vector -> (Float, Float)
projectPolygon verts axis = (minimum values, maximum values)
  where
    -- Para cada vértice, calculamos el producto escalar con el eje.
    -- Esto equivale a "proyectar" el vértice sobre ese eje.
    values = [dot v axis | v <- verts]


------------------------------------------------------------
-- Comprueba si dos intervalos (minA,maxA) y (minB,maxB)
-- se solapan. Devuelve True si hay intersección.
------------------------------------------------------------
overlaps :: (Float, Float) -> (Float, Float) -> Bool
overlaps (minA,maxA) (minB,maxB) = not (maxA < minB || maxB < minA)


------------------------------------------------------------
-- SAT (Separating Axis Theorem)
-- Devuelve True si dos polígonos convexos colisionan.
-- Se basa en proyectar ambos sobre todos los ejes normales
-- a sus aristas y comprobar si hay solapamiento en todos.
------------------------------------------------------------
satCollision :: [Point] -> [Point] -> Bool
satCollision verts1 verts2 =
  all checkAxis allAxes
  where
    -- Todos los ejes a comprobar = normales a las aristas de ambos polígonos
    allAxes = axes verts1 ++ axes verts2

    -- Genera los ejes perpendiculares a las aristas de un polígono
    axes poly = [perp (sub v2 v1) | (v1,v2) <- edges poly]

    -- Genera todas las aristas consecutivas (incluye la última con la primera)
    edges poly = zip poly (tail poly ++ [head poly])

    -- Normaliza un vector para usarlo como eje unitario
    normalize (x,y) = (x / sqrt (x*x + y*y), y / sqrt (x*x + y*y))

    -- Proyecta ambos polígonos sobre el eje normalizado y comprueba solapamiento
    checkAxis axis = overlaps (projectPolygon verts1 nAxis)
                               (projectPolygon verts2 nAxis)
      where
        nAxis = normalize axis


------------------------------------------------------------
-- Comprueba si dos rectángulos rotados se solapan
-- usando SAT sobre sus vértices.
------------------------------------------------------------
checkCollision :: Rectangulo -> Rectangulo -> Bool
checkCollision r1 r2 =
  satCollision (verticesRect r1) (verticesRect r2)


------------------------------------------------------------
-- Detecta colisiones entre robots y proyectiles.
-- Devuelve una lista de pares (robotId, proyectilId) que colisionan.
------------------------------------------------------------
detectRobotProjectileCollisions :: [Robot] -> [Proyectil] -> [(Int, Int)]
detectRobotProjectileCollisions robots projectiles =
  [ (objId r, objId p)| r <- robots, p <- projectiles, checkCollision (robotToRect r) (projToRect p)  
  -- Convierte Robot/Proyectil a Rectángulo para SAT
  ]


------------------------------------------------------------
-- Detecta colisiones entre robots entre sí.
-- Devuelve pares (id1, id2) para cada pareja que colisiona.
-- Usa tails para evitar duplicar parejas (1,2) y (2,1)
------------------------------------------------------------
detectRobotRobotCollisions :: [Robot] -> [(Int, Int)]
detectRobotRobotCollisions robots =
  [ (objId r1, objId r2)
  | (robot:lista_robots) <- tails robots
  , r2 <- lista_robots
  , checkCollision (robotToRect r1) (robotToRect r2)
  ]


------------------------------------------------------------
-- Función principal de colisiones.
-- Coordina las comprobaciones Robot-Robot y Robot-Proyectil
-- y devuelve ambas listas de colisiones detectadas.
------------------------------------------------------------
checkCollisions :: [Robot] -> [Proyectil] -> ([(Int, Int)], [(Int, Int)])
checkCollisions robots projectiles =
  (robotRobotCollisions, robotProjectileCollisions)
  where
    robotRobotCollisions      = detectRobotRobotCollisions robots
    robotProjectileCollisions = detectRobotProjectileCollisions robots projectiles
