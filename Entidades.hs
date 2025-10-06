module Entidades
  ( Point, Vector, Angle, Distance, Tiempo, Position, Velocity, Size, Scale, Objeto(..)
  , RobotData(..), ProyectilData(..), Robot(..), Proyectil(..), Action(..), GameState(..), BotAction(..)
  ) where
  

type Point = (Float, Float)

type Vector = (Float, Float)

type Angle = Float

type Distance = Float

type Tiempo = Float

type Position = (Float, Float)

type Velocity = (Float, Float)

type Size = (Float, Float)

type Scale = (Float, Float)

data Objeto a = 
    Objeto  {
        objectId :: Int,
        position :: Position,
        velocity :: Velocity,
        angulo :: Angle, 
        explosion :: Bool, 
        size :: Size,
        imagenObjeto :: String,
        extras :: a
    }
    deriving (Show, Eq)



data RobotData =
    RobotData {
        imagenCannon :: String,
        energy :: Float ,
        range :: Float 
        } 
        deriving (Show, Eq)

data ProyectilData = 
    ProyectilData {
      damage :: Float,
      existingTime :: Tiempo
    }
    deriving (Show, Eq)

type Robot = Objeto RobotData
type Proyectil = Objeto ProyectilData

data Action =
    Action{
      stop :: Bool,
      direction :: Vector,
      accelerate :: Float,
      shoot :: Bool
    }
    deriving (Show, Eq)

data GameState = 
  GameState {
    imagenM :: String,
    sizeM :: Size,
    nProyectiles :: Int,
    nTanques :: Int
  }
  deriving (Show, Eq)


data BotAction = 
      Move Position
    | Rotate Angle
    | Acelerate Float
    | Shoot
    | Stop
    | Combo [BotAction]


