module Route ( Route, newR, inOrderR )
  where

data Route = Rou [ String ] deriving (Eq, Show)

newR :: [ String ] -> Route                    -- construye una ruta segun una lista de ciudades
newR ciudades = Rou ciudades

inOrderR :: Route -> String -> String -> Bool  -- indica si la primer ciudad consultada esta antes que la segunda ciudad en la ruta
inOrderR (Rou ciudades) ciudad1 ciudad2 = checkOrder ciudades ciudad1 ciudad2
  where
    checkOrder [] _ _ = False
    checkOrder (x:xs) c1 c2
      | x == c1 = True
      | x == c2 = False
      | otherwise = checkOrder xs c1 c2
