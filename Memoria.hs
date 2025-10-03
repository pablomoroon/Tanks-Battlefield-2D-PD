module Memoria 
(Value(..),Memoria,vacia,
save,obtener,
delete,exist,upgrade,upgradeX1,
) where
--Creamos Un contenedor para todo tipo de datos
import qualified Data.Map.Strict as Map
import Entidades
import qualified Control.Monad.RWS.Class as Map
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
--Map String-Valor para guardar todo tipo de datos
type Memoria=Map.Map String Value


--Definicion de memoria vacía
vacia::Memoria
vacia = Map.empty


--Save, guardar un dato en la memoria
save::String->String->Value->Memoria
--Esquema: save clave val mem = Map.insert clave val mem
save=Map.insert


--Obtener datos de memoria
obtener:: String->Memoria->Maybe Value
--Esquema:obtener clave mem= Map.lookup clave mem
obtener=Map.lookup


--Delete,Bprrar datos en memoria
delete::String->Memoria->Memoria
--Esquema:delete clave mem=Map.delete clave mem
delete=Map.delete


-- Existe, nos dice si para una clave dada y una memoria, se encuentra dentro de la memoria
exist :: String -> Memoria -> Bool
--Esquema::exist clave mem = Map.member clave mem
exist =Map.member


--Upgrade, actualizar datos si existe la clave
--recibe clave funcion y memoria; busca la clave, si existe la clave le aplica 
--la función y la guarda y si no la deja igual
upgrade::String -> (Value -> Value) -> Memoria -> Memoria
upgrade clave f mem =
  case Map.lookup clave mem of
    Just v  -> Map.insert clave (f v) mem
    Nothing -> mem


-- upgradeX1, si f devuelve Nothing, se elimina la clave
--recibe clave funcion y memoria; busca la clave, si existe la clave le aplica 
--la función, que esta vez devuelve un Maybe value, si devuelve algo lo reemplaza 
--y si no no hace nada
upgradeX1 :: String -> (Value -> Maybe Value) -> Memoria -> Memoria
upgradeX1 clave f mem =
  case Map.lookup clave mem of
    Just v  -> maybe (Map.delete clave mem) (\v' -> Map.insert clave v' mem) (f v)