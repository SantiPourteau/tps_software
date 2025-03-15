module Route ( Route, newR, inOrderR )
  where

data Route = Rou [ String ] deriving (Eq, Show)

-- Construye una ruta a partir de una lista de ciudades.
newR :: [ String ] -> Route
newR [] = error "La lista de ciudades no puede estar vacía al crear la ruta"
newR ciudades = Rou ciudades

-- Indica si la primer ciudad (city1) aparece antes que la segunda (city2) en la ruta.
inOrderR :: Route -> String -> String -> Bool
inOrderR (Rou []) _ _ = error "La ruta no puede estar vacía"
inOrderR (Rou ciudades) city1 city2
  | not (city1 `elem` ciudades) = error ("La ciudad " ++ city1 ++ " no se encuentra en la ruta")
  | not (city2 `elem` ciudades) = error ("La ciudad " ++ city2 ++ " no se encuentra en la ruta")
  | otherwise = checkOrder ciudades
  where
    checkOrder (x:xs)
      | x == city1 = True
      | x == city2 = False
      | otherwise  = checkOrder xs
