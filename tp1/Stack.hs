module Stack ( Stack, newS, freeCellsS, stackS, netS, holdsS, popS )
  where

import Palet
import Route

data Stack = Sta [ Palet ] Int deriving (Eq, Show)
-- Capacidad lo tomamos como la cantidad de palets que puede tener la pila, no la cantidad disponibles restantes. Por lo tanto es un numero fijo una vez creado el stack.
-- Por el manejo de errores en Palet.hs y Route.hs, no es necesario manejar los errores de Palets y Routes en Stack.hs. Asumimos que los paletes y rutas son validos.

newS :: Int -> Stack                      -- construye una Pila con la capacidad indicada 
newS capacidad 
  | capacidad > 0  = Sta [] capacidad
  | otherwise = error "La capacidad debe ser mayor a 0"

freeCellsS :: Stack -> Int                -- responde la celdas disponibles en la pila
freeCellsS (Sta palets capacidad) = capacidad - length palets

stackS :: Stack -> Palet -> Stack         -- apila el palet indicado en la pila
stackS (Sta palets capacidad) palet = Sta (palet:palets) capacidad

netS :: Stack -> Int                      -- responde el peso neto de los paletes en la pila
netS (Sta palets _) = sum [netP p | p <- palets]

holdsS :: Stack -> Palet -> Route -> Bool
holdsS stack nuevoPalet route
  | freeCellsS stack <= 0 = False
  | not (inRouteR route (destinationP nuevoPalet)) = False
  | otherwise = case stack of
      -- Si la pila está vacía, basta con que el destino esté en la ruta
      Sta [] _ -> True

      -- Si la pila no está vacía, el destino del nuevo palet (cityN)
      -- debe visitarse *antes* que el del palet de arriba (cityTop),
      -- para que sea el primero en desapilarse.
      Sta (p:_) _ ->
          inOrderR route (destinationP nuevoPalet) (destinationP p)


popS :: Stack -> String -> Stack          -- quita del tope los paletes con destino en la ciudad indicada

-- Caso base: si la pila esta vacia, no se puede quitar ningun palet
popS (Sta [] capacidad) _ = Sta [] capacidad
-- Caso recursivo: si el palet en el tope de la pila tiene el destino indicado, se quita y se continua con el resto de la pila
popS (Sta (p:ps) capacidad) ciudad
    | destinationP p == ciudad = popS (Sta ps capacidad) ciudad
    | otherwise = Sta (p:ps) capacidad

-- Aclaracion: si el palet superior no tiene el destino indicado, se devuelve la pila sin cambios.





