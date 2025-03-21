module Main where

import Palet
import Route
import Stack
import Truck
import Control.Exception
import System.IO.Unsafe

-- | testF devuelve True si la acción dada falla (lanza una excepción), y False en caso contrario.
testF :: Show a => a -> Bool
testF action = unsafePerformIO $ do
    result <- tryJust isException (evaluate action)
    return $ case result of
        Left _ -> True
        Right _ -> False
  where
    isException :: SomeException -> Maybe ()
    isException _ = Just ()

-- | runTest recibe un nombre de test y una acción IO. Ejecuta la acción,
-- captura cualquier excepción, imprime si el test pasó o falló,
-- y luego continúa con el siguiente test.
runTest :: String -> IO () -> IO ()
runTest testName action = do
    putStrLn $ "\n---------------------------------------"
    putStrLn $ "Iniciando test: " ++ testName
    result <- try action
    case result of
         Left e  -> do
             putStrLn $ "El test \"" ++ testName ++ "\" falló con excepción: " ++ show (e :: SomeException)
             putStrLn "---------------------------------------"
         Right _ -> do
             putStrLn $ "El test \"" ++ testName ++ "\" pasó con éxito."
             putStrLn "---------------------------------------"

main :: IO ()
main = do
    putStrLn "Ejecutando todos los tests..."

    ----------------------------------------------------------------
    -- TESTS DE PALET
    runTest "Palet: Creación válida" $ do
         putStrLn "Creando Palet con destino 'CityA' y peso 5..."
         let p1 = newP "CityA" 5
         putStrLn $ "Palet creado = " ++ show p1
         if destinationP p1 /= "CityA"
            then error "destinationP falló para un Palet válido"
            else putStrLn "destinationP verificado correctamente."
         if netP p1 /= 5
            then error "netP falló para un Palet válido"
            else putStrLn "netP verificado correctamente."

    runTest "Palet: Error en newP con destino vacío" $ do
         putStrLn "Intentando crear un Palet con destino vacío y peso 5..."
         if not (testF (newP "" 5))
            then error "newP debería fallar cuando el destino está vacío"
            else putStrLn "newP falló correctamente con destino vacío."

    runTest "Palet: Error en newP con peso no positivo" $ do
         putStrLn "Intentando crear un Palet con destino 'CityA' y peso 0..."
         if not (testF (newP "CityA" 0))
            then error "newP debería fallar cuando el peso no es mayor que 0"
            else putStrLn "newP falló correctamente con peso <= 0."

    ----------------------------------------------------------------
    -- TESTS DE ROUTE
    runTest "Route: Orden válido con inOrderR" $ do
         putStrLn "Creando ruta con [CityA, CityB, CityC]..."
         let route = newR ["CityA", "CityB", "CityC"]
         putStrLn "Verificando que 'CityA' viene antes que 'CityB'..."
         if not (inOrderR route "CityA" "CityB")
            then error "inOrderR falló: 'CityA' debería venir antes que 'CityB'"
            else putStrLn "inOrderR OK para (CityA -> CityB)."
         putStrLn "Verificando que 'CityB' NO viene antes que 'CityA'..."
         if inOrderR route "CityB" "CityA"
            then error "inOrderR falló: 'CityB' no debería venir antes que 'CityA'"
            else putStrLn "inOrderR OK para negar (CityB -> CityA)."

    runTest "Route: Error en inOrderR con ciudad ausente" $ do
         let route = newR ["CityA", "CityB", "CityC"]
         putStrLn "Probando inOrderR con 'CityX' como primera ciudad..."
         if testF (inOrderR route "CityX" "CityA")
            then error "inOrderR debería devolver False si la primera ciudad no está en la ruta"
            else putStrLn "inOrderR devolvió False correctamente (primera ciudad ausente)."
         putStrLn "Probando inOrderR con 'CityX' como segunda ciudad..."
         if testF (inOrderR route "CityA" "CityX")
            then error "inOrderR debería devolver False si la segunda ciudad no está en la ruta"
            else putStrLn "inOrderR devolvió False correctamente (segunda ciudad ausente)."
    
    runTest "Route: inOrderR con ciudades intermedias" $ do
         let route = newR ["CityA", "CityX", "CityB", "CityC"]
         if not (inOrderR route "CityA" "CityB")
            then error "inOrderR falló con ciudades intermedias"
            else putStrLn "inOrderR OK con ciudades intermedias."

    runTest "Route: Error en newR con lista vacía" $ do
         putStrLn "Intentando crear una ruta vacía..."
         if not (testF (newR []))
            then error "newR debería fallar al recibir una lista vacía"
            else putStrLn "newR falló correctamente con lista vacía."

    runTest "Route: Error en newR por ciudades duplicadas" $ do
         putStrLn "Intentando crear una ruta con ciudades duplicadas [CityA, CityB, CityA]..."
         if not (testF (newR ["CityA", "CityB", "CityA"]))
            then error "newR debería fallar con ciudades duplicadas"
            else putStrLn "newR falló correctamente al detectar duplicados."

    ----------------------------------------------------------------
    -- TESTS DE STACK
    runTest "Stack: freeCellsS y netS" $ do
         putStrLn "Creando un Palet destino 'CityA', peso 5..."
         let p1 = newP "CityA" 5
         putStrLn "Creando stack con capacidad 2..."
         let s = newS 2
         putStrLn $ "Stack inicial: " ++ show s
         putStrLn $ "Celdas libres iniciales (se esperaba 2): " ++ show (freeCellsS s)
         if freeCellsS s /= 2
            then error "freeCellsS incorrecto en stack vacío"
            else putStrLn "freeCellsS verificado correctamente."
         putStrLn "Apilando Palet p1 en stack..."
         let s1 = stackS s p1
         putStrLn $ "Stack tras apilar p1: " ++ show s1
         putStrLn $ "Celdas libres tras apilar (se esperaba 1): " ++ show (freeCellsS s1)
         if freeCellsS s1 /= 1
            then error "freeCellsS incorrecto tras apilar"
            else putStrLn "freeCellsS verificado tras un push."
         putStrLn $ "Peso neto tras apilar (se esperaba 5): " ++ show (netS s1)
         if netS s1 /= 5
            then error "netS incorrecto tras apilar"
            else putStrLn "netS verificado tras un push."

    runTest "Stack: holdsS en stack vacío" $ do
         let route = newR ["CityA", "CityB", "CityC"]
         let s = newS 2
         putStrLn $ "Stack vacío: " ++ show s
         putStrLn "Verificando holdsS con Palet destino 'CityA'..."
         let pA = newP "CityA" 3
         putStrLn $ "Palet a probar: " ++ show pA
         if not (holdsS s pA route)
            then error "holdsS falló para stack vacío con destino válido"
            else putStrLn "holdsS funcionó bien para stack vacío."

    runTest "Stack: holdsS con orden válido" $ do
         let route = newR ["CityA", "CityB", "CityC"]
         let s = newS 2
         let pB = newP "CityB" 5
         let validP = newP "CityA" 4
         putStrLn $ "Stack inicial: " ++ show s
         putStrLn $ "Apilando Palet destino 'CityB' -> " ++ show pB
         let s1 = stackS s pB
         putStrLn $ "Stack actual: " ++ show s1
         putStrLn $ "Probando holdsS con palet destino 'CityA' -> " ++ show validP
         if not (holdsS s1 validP route)
            then error "holdsS falló en orden válido (CityA -> CityB)"
            else putStrLn "holdsS verificó el orden (CityA -> CityB)."

    runTest "Stack: holdsS falla en orden inválido" $ do
         let route = newR ["CityA", "CityB", "CityC"]
         let s = newS 2
         let s1 = stackS s (newP "CityA" 5)
         let s2 = stackS s1 (newP "CityB" 4)
         putStrLn $ "Stack tras apilar 'CityA' y luego 'CityB': " ++ show s2
         putStrLn "Intentando apilar palet destino 'CityA' sobre 'CityB'..."
         if holdsS s2 (newP "CityA" 2) route
            then error "holdsS debería fallar por orden inválido (CityB -> CityA)"
            else putStrLn "holdsS falló correctamente con orden inválido (CityB -> CityA)."

    runTest "Stack: holdsS retorna False en stack lleno" $ do
         let route = newR ["CityA", "CityB", "CityC"]
         let s = newS 2
         let s1 = stackS s (newP "CityA" 5)
         let s2 = stackS s1 (newP "CityB" 4)
         putStrLn $ "Stack lleno: " ++ show s2
         putStrLn $ "Celdas libres (se esperaba 0): " ++ show (freeCellsS s2)
         if freeCellsS s2 /= 0
            then error "El stack debería estar lleno"
            else putStrLn "Stack lleno como se esperaba."
         putStrLn "Probando holdsS con nuevo palet destino 'CityC'..."
         if holdsS s2 (newP "CityC" 1) route
            then error "holdsS debería retornar False en stack lleno"
            else putStrLn "holdsS retornó False correctamente en stack lleno."

    runTest "Stack: popS remueve palets correctos" $ do
         putStrLn "Creando stack de capacidad 3 con palets: CityB, CityA, CityB..."
         let s = newS 3
         let s3a = stackS s (newP "CityB" 4)
         let s3b = stackS s3a (newP "CityA" 5)
         let s3c = stackS s3b (newP "CityB" 3)
         putStrLn $ "Stack antes del pop: " ++ show s3c
         putStrLn $ "Celdas libres antes del pop: " ++ show (freeCellsS s3c)
         putStrLn "Aplicando popS para 'CityB'..."
         let s3p = popS s3c "CityB"
         putStrLn $ "Stack después del pop: " ++ show s3p
         putStrLn $ "Celdas libres después del pop: " ++ show (freeCellsS s3p)
         if freeCellsS s3p /= 1
            then error "popS no removió los palets correctamente (freeCellsS)"
            else putStrLn "popS removió palets 'CityB' correctamente."
         putStrLn $ "Peso neto después del pop (se esperaba 9): " ++ show (netS s3p)
         if netS s3p /= (5 + 4)
            then error "popS no actualizó netS correctamente"
            else putStrLn "popS actualizó netS correctamente."

    runTest "Stack: popS detiene en el primer palet no coincidente" $ do
         -- Creamos un stack de capacidad 4 con palets (de arriba a abajo): [CityB, CityB, CityA, CityB]
         let s = newS 4
         let s1 = stackS s (newP "CityB" 2)   -- tope
         let s2 = stackS s1 (newP "CityB" 3)
         let s3 = stackS s2 (newP "CityA" 4)
         let s4 = stackS s3 (newP "CityB" 5)   -- fondo
         -- Al aplicar popS para "CityB", solo deben removerse los dos primeros palets.
         let sPop = popS s4 "CityB"
         if netS sPop /= (4 + 5)
            then error "popS no se detuvo en el primer palet no coincidente"
            else putStrLn "popS se detuvo correctamente al encontrar un palet no coincidente."

    ----------------------------------------------------------------
    -- TESTS DE TRUCK
    runTest "Truck: freeCellsT y carga inicial" $ do
         putStrLn "Creando camión con 2 bahías, altura 2, ruta [CityA, CityB, CityC]..."
         let truckRoute = newR ["CityA", "CityB", "CityC"]
         let truck = newT 2 2 truckRoute  -- 2 bahías, 2 palets cada una (total 4 celdas)
         putStrLn $ "Celdas libres (se esperaba 4): " ++ show (freeCellsT truck)
         if freeCellsT truck /= 4
            then error "freeCellsT incorrecto en camión nuevo"
            else putStrLn "freeCellsT verificado en camión nuevo."
         putStrLn "Cargando Palet destino 'CityA' (peso 3)..."
         let t1 = loadT truck (newP "CityA" 3)
         putStrLn $ "Celdas libres tras la primera carga (se esperaba 3): " ++ show (freeCellsT t1)
         if freeCellsT t1 /= 3
            then error "freeCellsT incorrecto tras cargar un palet"
            else putStrLn "freeCellsT verificado tras la primera carga."
         putStrLn "Cargando Palet destino 'CityB' (peso 4)..."
         let t2 = loadT t1 (newP "CityB" 4)
         putStrLn $ "Peso neto del camión (se esperaba 7): " ++ show (netT t2)
         if netT t2 /= 7
            then error "netT incorrecto tras dos cargas"
            else putStrLn "netT verificado tras dos cargas."

    runTest "Truck: orden inválido en camión de 1 bahía" $ do
         putStrLn "Creando camión con 1 bahía, altura 2, ruta [CityA, CityB, CityC]..."
         let truckRoute = newR ["CityA", "CityB", "CityC"]
         let truckSingle = newT 1 2 truckRoute
         putStrLn "Cargando Palet destino 'CityA' (peso 3)..."
         let t1 = loadT truckSingle (newP "CityA" 3)
         putStrLn "Cargando Palet destino 'CityB' (peso 4)..."
         let t2 = loadT t1 (newP "CityB" 4)
         putStrLn "Intentando cargar Palet destino 'CityA' (peso 2) sobre palet de 'CityB'..."
         if not (testF (loadT t2 (newP "CityA" 2)))
            then error "loadT debería fallar por orden inválido (no se puede cargar CityA después de CityB)"
            else putStrLn "loadT falló correctamente con orden inválido."
    
    runTest "Truck: capacidad del camión" $ do
         putStrLn "Creando camión con 2 bahías, altura 2, ruta [CityA, CityB, CityC]..."
         let truckRoute = newR ["CityA", "CityB", "CityC"]
         let truck = newT 2 2 truckRoute
         putStrLn "Cargando 4 palets para llenar el camión..."
         let t1 = loadT truck (newP "CityC" 3)
         let t2 = loadT t1 (newP "CityB" 4)
         let t3 = loadT t2 (newP "CityA" 2)
         let t4 = loadT t3 (newP "CityA" 1)
         putStrLn $ "Celdas libres tras 4 cargas (se esperaba 0): " ++ show (freeCellsT t4)
         if freeCellsT t4 /= 0
            then error "El camión no está lleno tras 4 cargas"
            else putStrLn "El camión está lleno como se esperaba."
         putStrLn "Intentando cargar un palet adicional..."
         if not (testF (loadT t4 (newP "CityA" 1)))
            then error "loadT debería fallar cuando el camión está lleno"
            else putStrLn "loadT falló correctamente al no haber espacio."

    runTest "Truck: unloadT remueve palets" $ do
         putStrLn "Creando camión con 2 bahías, altura 3, ruta [CityA, CityB, CityC]..."
         let truckRoute = newR ["CityA", "CityB", "CityC"]
         let truck2 = newT 2 3 truckRoute
         putStrLn "Cargando Palet destino 'CityA' (peso 2)..."
         let truck2a = loadT truck2 (newP "CityA" 2)
         putStrLn "Cargando Palet destino 'CityB' (peso 3)..."
         let truck2b = loadT truck2a (newP "CityB" 3)
         putStrLn "Cargando Palet destino 'CityA' (peso 4)..."
         let truck2c = loadT truck2b (newP "CityA" 4)
         putStrLn $ "Peso neto antes de descargar (se esperaba 9): " ++ show (netT truck2c)
         putStrLn "Descargando palets con destino 'CityA'..."
         let truck2u = unloadT truck2c "CityA"
         putStrLn $ "Peso neto después de descargar (se esperaba 3): " ++ show (netT truck2u)
         if netT truck2u /= 3
            then error "unloadT no removió correctamente los palets de 'CityA'"
            else putStrLn "unloadT removió los palets de 'CityA' correctamente."
    
    runTest "Truck: límite de peso en bahía" $ do
         putStrLn "Creando camión con 1 bahía de capacidad 3, ruta [CityA, CityB, CityC]..."
         let route = newR ["CityA", "CityB", "CityC"]
         let truckSingle = newT 1 3 route
         putStrLn "Cargando palets hasta alcanzar casi 10 toneladas..."
         let t1 = loadT truckSingle (newP "CityA" 5)  -- peso = 5
         let t2 = loadT t1 (newP "CityB" 4)             -- peso = 9 (5+4)
         putStrLn "Intentando cargar un palet de 2 toneladas (excedería el límite, 9+2=11)..."
         if not (testF (loadT t2 (newP "CityC" 2)))
            then error "loadT debería fallar al exceder el límite de 10 toneladas en la bahía"
            else putStrLn "loadT falló correctamente al exceder el límite de peso."

    runTest "Truck: loadT falla si destino no está en la ruta" $ do
         let route = newR ["CityA", "CityB", "CityC"]
         let truck = newT 2 3 route
         putStrLn "Intentando cargar un palet con destino 'CityX' (no está en la ruta)..."
         if not (testF (loadT truck (newP "CityX" 2)))
            then error "loadT debería fallar si el destino del palet no está en la ruta"
            else putStrLn "loadT falló correctamente al recibir un destino fuera de la ruta."

    putStrLn "\n¡Todos los tests se han ejecutado!"
