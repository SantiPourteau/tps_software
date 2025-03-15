module Palet ( Palet, newP, destinationP, netP )
  where

import Control.Exception
import System.IO.Unsafe

data Palet = Pal String Int deriving (Eq, Show)

newP :: String -> Int -> Palet
newP destino peso 
  | destino == "" = error "El destino no puede ser vacío"
  | peso > 0  = Pal destino peso
  | otherwise = error "El peso del palet debe ser mayor a 0"

destinationP :: Palet -> String  -- responde la ciudad destino del palet
destinationP (Pal ciudad _) = ciudad

netP :: Palet -> Int             -- responde el peso en toneladas del palet
netP (Pal _ peso) = peso
