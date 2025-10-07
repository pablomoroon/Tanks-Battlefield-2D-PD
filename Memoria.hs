module Memoria 
(Value(..),Memoria,vacia,
save,obtener,
delete,exist,
) where

import qualified Data.Map.Strict as Map
import Entidades
 
data Value = VInt Int
  | VFloat Float
  | VString String
  | VBool Bool
  | VPoint Point
  | VVector Vector
  | VRobot Robot
  | VProyectil Proyectil
  | VGameState GameState
  | VList [Value]
  deriving (Show, Eq)

type Memoria=Map.Map String Value

vacia::Memoria
vacia = Map.empty

save::String->Value->Memoria->Memoria
save=Map.insert


obtener:: String->Memoria->Maybe Value
obtener=Map.lookup

delete::String->Memoria->Memoria
delete=Map.delete

exist :: String -> Memoria -> Bool
exist =Map.member
