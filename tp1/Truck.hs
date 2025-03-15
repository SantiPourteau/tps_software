module Truck ( Truck, newT, freeCellsT, loadT, unloadT, netT )
  where

import Palet
import Stack
import Route

data Truck = Tru [ Stack ] Route deriving (Eq, Show)

newT :: Int -> Int -> Route -> Truck  -- construye un camion según una cantidad de bahias, la altura de las mismas y una ruta
newT bahias altura ruta
  | bahias <= 0 = error "La cantidad de bahías debe ser mayor a 0"
  -- | altura <= 0 = error "La altura de las bahías debe ser mayor a 0"
  -- ya se controla en newS
  | otherwise = Tru (replicate bahias (newS altura)) ruta

freeCellsT :: Truck -> Int            -- responde la celdas disponibles en el camion
-- La cantidad celdas disponibles es la cantidad maxima de paletes que se pueden cargar en el camion, teniendo en cuenta peso y capacidad??
-- O es la cantidad de celdas vacias en el camion? (por ahora usamos esta)
freeCellsT (Tru stacks _) = sum [freeCellsS stack | stack <- stacks]

loadT :: Truck -> Palet -> Truck      -- carga un palet en el camion
-- no cargo el palet si: al agregarlo a una bahía excede el limite de peso (10 toneladas), si excede la capacidad, si no hay bahias disponibles, 
-- si el destino del palet no es una ciudad de la ruta, si el destino del palet es anterior al destino del palet que esta en el tope de la pila


unloadT :: Truck -> String -> Truck   -- responde un camion al que se le han descargado los paletes que podían descargarse en la ciudad
unloadT (Tru stacks ruta) ciudad = Tru [popS stack ciudad | stack <- stacks] ruta

netT :: Truck -> Int                  -- responde el peso neto en toneladas de los paletes en el camion
netT (Tru stacks _) = sum [netS stack | stack <- stacks]

