import Palet
import Route
import Stack
import Truck

main :: IO ()
main = do
    putStrLn "Ejecutando pruebas..."
    
    -- Pruebas de Palet
    let palet1 = newP "Buenos Aires" 10
    let palet2 = newP "Córdoba" 15
    putStrLn $ "Destino palet1: " ++ destinationP palet1
    putStrLn $ "Peso palet1: " ++ show (netP palet1)
    
    -- Pruebas de Route
    let ruta = newR ["Buenos Aires", "Córdoba", "Mendoza"]
    putStrLn $ "¿BA está antes que Córdoba?: " ++ show (inOrderR ruta "Buenos Aires" "Córdoba")
    putStrLn $ "¿Córdoba está antes que BA?: " ++ show (inOrderR ruta "Córdoba" "Buenos Aires")
    
    -- Pruebas de Stack
    let stack = newS 3
    let stack1 = stackS stack palet1
    let stack2 = stackS stack1 palet2
    putStrLn $ "Celdas libres: " ++ show (freeCellsS stack2)
    putStrLn $ "Peso neto stack: " ++ show (netS stack2)
    
    -- Pruebas de Truck
    let truck = newT 2 3 ruta
    let truck1 = loadT truck palet1
    let truck2 = loadT truck1 palet2
    putStrLn $ "Celdas libres camión: " ++ show (freeCellsT truck2)
    putStrLn $ "Peso neto camión: " ++ show (netT truck2)
    
    putStrLn "Pruebas completadas!" 