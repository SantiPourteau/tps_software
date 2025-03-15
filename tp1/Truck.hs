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
freeCellsT (Tru stacks _) = sum [freeCellsS stack | stack <- stacks]

loadT :: Truck -> Palet -> Truck      -- carga un palet en el camion
loadT (Tru stacks ruta) palet = Tru (cargarEnMejorStack stacks palet ruta) ruta
  where
    cargarEnMejorStack [] _ _ = []
    cargarEnMejorStack (s:ss) p r
      | freeCellsS s > 0 && holdsS s p r = stackS s p : ss
      | otherwise = s : cargarEnMejorStack ss p r

unloadT :: Truck -> String -> Truck   -- responde un camion al que se le han descargado los paletes que podían descargarse en la ciudad
unloadT (Tru stacks ruta) ciudad = Tru [popS stack ciudad | stack <- stacks] ruta

netT :: Truck -> Int                  -- responde el peso neto en toneladas de los paletes en el camion
netT (Tru stacks _) = sum [netS stack | stack <- stacks]

