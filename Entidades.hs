{-# LANGUAGE DeriveFunctor #-}

module Entidades
  ( Vec2(..), (^+^), (^-^), (^*), (*^)
  , Point, Vector, Angle, Distance, Tiempo, Position, Velocity, Size, Scale
  , Objeto(..), RobotData(..), ProyectilData(..), Robot, Proyectil
  , TipoRobot(..)
  , Action(..), GameState(..), BotAction(..)
  ) where


import Control.Applicative (liftA2)

data Vec2 a = V2 { vx :: a, vy :: a }
  deriving (Eq, Show, Functor)

instance Applicative Vec2 where
  pure a = V2 a a
  V2 fx fy <*> V2 x y = V2 (fx x) (fy y)

infixl 6 ^+^, ^-^
infixl 7 ^*, *^

(^+^) :: Num a => Vec2 a -> Vec2 a -> Vec2 a
(^+^) = liftA2 (+)

( ^-^ ) :: Num a => Vec2 a -> Vec2 a -> Vec2 a
( ^-^ ) = liftA2 (-)

(^*) :: Num a => Vec2 a -> a -> Vec2 a
V2 x y ^* s = V2 (x*s) (y*s)

(*^) :: Num a => a -> Vec2 a -> Vec2 a
s *^ V2 x y = V2 (s*x) (s*y)

type Point    = Vec2 Float 
type Vector   = Vec2 Float
type Angle    = Float
type Distance = Float
type Tiempo   = Float
type Position = Vec2 Float
type Velocity = Vec2 Float
type Size     = Vec2 Float
type Scale    = Vec2 Float

data Objeto a = Objeto
  { objectId      :: Int
  , position      :: Position
  , velocity      :: Velocity
  , angulo        :: Angle
  , anguloCanon   :: Angle
  , explosion     :: Bool
  , explosionTime :: Float
  , size          :: Size
  , imagenObjeto  :: String
  , extras        :: a
  } deriving (Show, Eq, Functor)

-- MODIFICADO: Ahora solo Humano y Zombie
data TipoRobot = Humano | Zombie deriving (Show, Eq)

data RobotData = RobotData
  { name   :: String
  , energy :: Float
  , range  :: Distance
  , speed  :: Float
  , tipo :: TipoRobot
  , memTarget :: Maybe Int        -- Id del objetivo asignado (si existe)
  , memRole   :: Maybe String     -- Rol táctico: "leader", "flank", "chase", etc.
  , memLastSeen :: Maybe Position -- Última posición vista del objetivo
  , memAggroCooldown :: Int       -- Cooldown para re-asignar/agitar agresividad
  } deriving (Show, Eq)

data ProyectilData = ProyectilData
  { damage   :: Float
  , ownerId  :: Int
  } deriving (Show, Eq)

type Robot     = Objeto RobotData
type Proyectil = Objeto ProyectilData

data Action = Action
  { stop      :: Bool
  , direction :: Vector
  , accel     :: Float
  , shoot     :: Bool
  } deriving (Show, Eq)

data GameState = GameState
  { worldSize :: Size
  , nTanques  :: Int
  } deriving (Show, Eq)

data BotAction
    = Move Position
    | Rotate Angle
    | RotateCannon Angle
    | Accelerate Float
    | Shoot
    | Stop
  | SetTarget (Maybe Int)        -- Guardar/limpiar objetivo en la memoria del robot
  | SetRole (Maybe String)       -- Asignar un rol táctico
  | UpdateLastSeen Position      -- Actualizar última posición vista del objetivo
  | SetAggroCooldown Int         -- Establecer cooldown/agresividad
    | Combo [BotAction]
  deriving (Show, Eq)