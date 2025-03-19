module Truck ( Truck, newT, freeCellsT, loadT, unloadT, netT )
  where

import Palet
import Stack
import Route

data Truck = Tru [ Stack ] Route deriving (Eq, Show)

newT :: Int -> Int -> Route -> Truck  -- construye un camion según una cantidad de bahias, la altura de las mismas y una ruta
newT bahias altura ruta = Tru (replicate bahias (newS altura)) ruta

freeCellsT :: Truck -> Int            -- responde la celdas disponibles en el camion
freeCellsT (Tru stacks _) = sum [freeCellsS stack | stack <- stacks]

loadT :: Truck -> Palet -> Truck
loadT (Tru stacks ruta) palet = 
  case cargarEnMejorStack stacks palet ruta of
    Just newStacks -> Tru newStacks ruta
    Nothing -> error "No hay espacio disponible para cargar el palet"
  where
    cargarEnMejorStack [] _ _ = Nothing
    cargarEnMejorStack (s:ss) p r
      | freeCellsS s > 0 
        && holdsS s p r 
        && netS s + netP p <= 10 = Just (stackS s p : ss)
      | otherwise = case cargarEnMejorStack ss p r of
                      Just rest -> Just (s : rest)
                      Nothing -> Nothing

-- unloadT (Tru stacks (Rou ciudades)) ciudad =
--     Tru [popS stack ciudad | stack <- stacks] (Rou (filter (/= ciudad) ciudades))

unloadT (Tru stacks ruta) ciudad = Tru [popS stack ciudad | stack <- stacks] ruta

-- Una vez descargado los paletes de la ciudad, que se elimine la ciudad de la ruta

netT :: Truck -> Int                  -- responde el peso neto en toneladas de los paletes en el camion
netT (Tru stacks _) = sum [netS stack | stack <- stacks]

