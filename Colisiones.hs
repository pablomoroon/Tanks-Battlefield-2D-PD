import Entidades
import Fisicas
import Robot

-- Proyecta un polígono sobre un eje
projectPolygon :: [Point] -> Vector -> (Float, Float)
projectPolygon verts axis = (minimum values, maximum values)
  where
    values = [dot v axis | v <- verts]


-- Comprueba si dos intervalos se solapan
overlaps :: (Float, Float) -> (Float, Float) -> Bool
overlaps (minA,maxA) (minB,maxB) = not (maxA < minB || maxB < minA)


-- SAT: devuelve True si colisionan
satCollision :: [Point] -> [Point] -> Bool
satCollision verts1 verts2 =
  all checkAxis allAxes
  where
    allAxes = axes verts1 ++ axes verts2

    axes poly = [perp (sub v2 v1) | (v1,v2) <- edges poly]

    edges poly = zip poly (tail poly ++ [head poly])

    normalize (x,y) = (x / sqrt (x*x + y*y), y / sqrt (x*x + y*y))

    checkAxis axis = overlaps (projectPolygon verts1 nAxis)
                               (projectPolygon verts2 nAxis)
      where
        nAxis = normalize axis



-- Comprueba si dos rectángulos rotados se solapan → True si colisionan
checkCollision :: Rectangulo -> Rectangulo -> Bool
checkCollision r1 r2 =
  satCollision (verticesRect r1) (verticesRect r2)
 