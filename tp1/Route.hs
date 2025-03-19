module Route ( Route, newR, inOrderR, inRouteR )
  where

data Route = Rou [ String ] deriving (Eq, Show)

-- Construye una ruta a partir de una lista de ciudades.
newR :: [ String ] -> Route
newR [] = error "La lista de ciudades no puede estar vacía al crear la ruta"
newR ciudades = Rou ciudades

inRouteR :: Route -> String -> Bool -- indica si la ciudad consultada está en la ruta
inRouteR (Rou ciudades) ciudad = ciudad `elem` ciudades

-- Indica si la primer ciudad (city1) aparece antes que la segunda (city2) en la ruta.
inOrderR :: Route -> String -> String -> Bool
inOrderR ruta city1 city2
  | not (inRouteR ruta city1) = error ("La ciudad " ++ city1 ++ " no se encuentra en la ruta")
  | not (inRouteR ruta city2) = error ("La ciudad " ++ city2 ++ " no se encuentra en la ruta")
  | otherwise = checkOrder ciudades
  where
    (Rou ciudades) = ruta
    checkOrder (x:xs)
      | x == city1 = True
      | x == city2 = False
      | otherwise  = checkOrder xs
