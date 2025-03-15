module Stack ( Stack, newS, freeCellsS, stackS, netS, holdsS, popS )
  where

import Palet
import Route
import qualified Control.Applicative as stack

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

holdsS :: Stack -> Palet -> Route -> Bool -- indica si la pila puede aceptar el palet considerando las ciudades en la ruta
holdsS (Sta (p:ps) _) nuevoPalet (Rou ciudades)
  | destinationP nuevoPalet `notElem` ciudades = False
  | otherwise = inOrderR (Rou ciudades) (destinationP nuevoPalet) (destinationP p)


popS :: Stack -> String -> Stack          -- quita del tope los paletes con destino en la ciudad indicada
popS (Sta [] capacidad) _ = Sta [] capacidad
popS (Sta (p:ps) capacidad) ciudad
    | destinationP p == ciudad = popS (Sta ps capacidad) ciudad
    | otherwise = Sta (p:ps) capacidad
