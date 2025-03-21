module Main where

import Palet
import Route
import Stack
import Truck
import Control.Exception
import System.IO.Unsafe

-- | testF returns True if the given action fails (raises an exception), False otherwise.
testF :: Show a => a -> Bool
testF action = unsafePerformIO $ do
    result <- tryJust isException (evaluate action)
    return $ case result of
        Left _ -> True
        Right _ -> False
  where
    isException :: SomeException -> Maybe ()
    isException _ = Just ()

-- | runTest takes a test name and an IO action. It runs the action,
-- catches any exceptions, prints whether the test passed or failed,
-- and then continues with the next test.
runTest :: String -> IO () -> IO ()
runTest testName action = do
    putStrLn $ "\n---------------------------------------"
    putStrLn $ "Iniciando test: " ++ testName
    result <- try action
    case result of
         Left e  -> do
             putStrLn $ "Test \"" ++ testName ++ "\" falló con excepción: " ++ show (e :: SomeException)
             putStrLn "---------------------------------------"
         Right _ -> do
             putStrLn $ "Test \"" ++ testName ++ "\" pasó con éxito."
             putStrLn "---------------------------------------"

main :: IO ()
main = do
    putStrLn "Ejecutando todos los tests..."

    ----------------------------------------------------------------
    -- PALLET TESTS
    runTest "Palet: Valid creation" $ do
         putStrLn "Creando Palet con destino 'CityA' y peso 5..."
         let p1 = newP "CityA" 5
         putStrLn $ "Palet creado = " ++ show p1
         if destinationP p1 /= "CityA"
            then error "destinationP falló para Palet válido"
            else putStrLn "destinationP verificado correctamente."
         if netP p1 /= 5
            then error "netP falló para Palet válido"
            else putStrLn "netP verificado correctamente."

    runTest "Palet: newP error on empty destination" $ do
         putStrLn "Intentando crear un Palet con destino vacío y peso 5..."
         if not (testF (newP "" 5))
            then error "newP debería fallar cuando el destino está vacío"
            else putStrLn "newP falló correctamente al pasar destino vacío."

    runTest "Palet: newP error on non-positive weight" $ do
         putStrLn "Intentando crear un Palet con destino 'CityA' y peso 0..."
         if not (testF (newP "CityA" 0))
            then error "newP debería fallar cuando el peso no es > 0"
            else putStrLn "newP falló correctamente al pasar peso <= 0."

    ----------------------------------------------------------------
    -- ROUTE TESTS
    runTest "Route: inOrderR valid ordering" $ do
         putStrLn "Creando una ruta con [CityA, CityB, CityC]..."
         let route = newR ["CityA", "CityB", "CityC"]
         putStrLn "Verificando que 'CityA' viene antes que 'CityB'..."
         if not (inOrderR route "CityA" "CityB")
            then error "inOrderR falló: 'CityA' debería venir antes que 'CityB'"
            else putStrLn "inOrderR OK para (CityA -> CityB)."
         putStrLn "Verificando que 'CityB' NO viene antes que 'CityA'..."
         if inOrderR route "CityB" "CityA"
            then error "inOrderR falló: 'CityB' no debería venir antes que 'CityA'"
            else putStrLn "inOrderR OK para negar (CityB -> CityA)."

    runTest "Route: inOrderR error for missing city" $ do
         let route = newR ["CityA", "CityB", "CityC"]
         putStrLn "Probando inOrderR con ciudad inexistente 'CityX' como primera ciudad..."
         if not (testF (inOrderR route "CityX" "CityA"))
            then error "inOrderR debería fallar si la primera ciudad no está en la ruta"
            else putStrLn "inOrderR falló correctamente cuando la primera ciudad no está en la ruta."
         putStrLn "Probando inOrderR con ciudad inexistente 'CityX' como segunda ciudad..."
         if not (testF (inOrderR route "CityA" "CityX"))
            then error "inOrderR debería fallar si la segunda ciudad no está en la ruta"
            else putStrLn "inOrderR falló correctamente cuando la segunda ciudad no está en la ruta."

    runTest "Route: newR error on empty list" $ do
         putStrLn "Intentando crear una ruta vacía..."
         if not (testF (newR []))
            then error "newR debería fallar si se pasa una lista vacía"
            else putStrLn "newR falló correctamente al pasar lista vacía."

    ----------------------------------------------------------------
    -- STACK TESTS
    runTest "Stack: freeCellsS and netS" $ do
         putStrLn "Creando un Palet destino 'CityA', peso 5..."
         let p1 = newP "CityA" 5
         putStrLn "Creando stack con capacidad 2..."
         let s = newS 2
         putStrLn $ "Stack inicial: " ++ show s
         putStrLn $ "Celdas libres iniciales (esperado 2): " ++ show (freeCellsS s)
         if freeCellsS s /= 2
            then error "freeCellsS incorrecto en stack vacío"
            else putStrLn "freeCellsS verificado correctamente para stack vacío."
         putStrLn "Apilando Palet p1 en stack..."
         let s1 = stackS s p1
         putStrLn $ "Stack tras apilar p1: " ++ show s1
         putStrLn $ "Celdas libres tras apilar (esperado 1): " ++ show (freeCellsS s1)
         if freeCellsS s1 /= 1
            then error "freeCellsS incorrecto tras apilar."
            else putStrLn "freeCellsS verificado correctamente tras un push."
         putStrLn $ "Peso neto tras apilar (esperado 5): " ++ show (netS s1)
         if netS s1 /= 5
            then error "netS incorrecto tras apilar."
            else putStrLn "netS verificado correctamente tras un push."

    runTest "Stack: holdsS valid for empty stack" $ do
         let route = newR ["CityA", "CityB", "CityC"]
         let s = newS 2
         putStrLn $ "Stack vacío: " ++ show s
         putStrLn "Verificando holdsS en una pila vacía con Palet destino 'CityA'..."
         let pA = newP "CityA" 3
         putStrLn $ "Palet a probar: " ++ show pA
         if not (holdsS s pA route)
            then error "holdsS falló para stack vacío con ciudad válida."
            else putStrLn "holdsS correcto para stack vacío y palet con destino válido."

    runTest "Stack: holdsS valid ordering" $ do
         let route = newR ["CityA", "CityB", "CityC"]
         let s = newS 2
         let pB = newP "CityB" 5
         let validP = newP "CityA" 4
         putStrLn $ "Stack inicial: " ++ show s
         putStrLn $ "Apilando Palet destino 'CityB' -> " ++ show pB
         let s1 = stackS s pB
         putStrLn $ "Stack ahora: " ++ show s1
         putStrLn $ "Probando holdsS con palet destino 'CityB' -> " ++ show validP
         if not (holdsS s1 validP route)
            then error "holdsS falló para un orden válido (CityA -> CityB)."
            else putStrLn "holdsS correcto para orden (CityA -> CityB)."

    runTest "Stack: holdsS fails for invalid ordering" $ do
         let route = newR ["CityA", "CityB", "CityC"]
         let s = newS 2
         let s1 = stackS s (newP "CityA" 5)
         let s2 = stackS s1 (newP "CityB" 4)
         putStrLn $ "Stack tras apilar CityA y luego CityB: " ++ show s2
         putStrLn "Intentando apilar un palet destino 'CityA' sobre 'CityB'..."
         if holdsS s2 (newP "CityA" 2) route
            then error "holdsS debería fallar por orden inválido (CityB -> CityA)."
            else putStrLn "holdsS falló correctamente cuando el orden es inválido (CityB -> CityA)."

    runTest "Stack: holdsS returns False on full stack" $ do
         let route = newR ["CityA", "CityB", "CityC"]
         let s = newS 2
         let s1 = stackS s (newP "CityA" 5)
         let s2 = stackS s1 (newP "CityB" 4)
         putStrLn $ "Stack lleno: " ++ show s2
         putStrLn $ "Celdas libres en s2 (esperado 0): " ++ show (freeCellsS s2)
         if freeCellsS s2 /= 0
            then error "La pila debería estar llena."
            else putStrLn "La pila está llena, como se esperaba."
         putStrLn "Intentando hacer holdsS con un nuevo palet destino 'CityC'..."
         if holdsS s2 (newP "CityC" 1) route
            then error "holdsS debería retornar False si la pila está llena."
            else putStrLn "holdsS retornó False correctamente cuando la pila está llena."

    runTest "Stack: popS removes correct palets" $ do
         putStrLn "Creando pila de capacidad 3 y apilando palets con destino CityB, CityA, CityB..."
         let s = newS 3
         let s3a = stackS s (newP "CityB" 4)
         let s3b = stackS s3a (newP "CityA" 5)
         let s3c = stackS s3b (newP "CityB" 3)
         putStrLn $ "Stack final antes del pop: " ++ show s3c
         putStrLn $ "Celdas libres antes del pop: " ++ show (freeCellsS s3c)
         putStrLn "Aplicando popS para 'CityB'..."
         let s3p = popS s3c "CityB"
         putStrLn $ "Stack después del pop: " ++ show s3p
         putStrLn $ "Celdas libres después del pop: " ++ show (freeCellsS s3p)
         if freeCellsS s3p /= 1
            then error "popS no removió los palets correctamente (freeCellsS)."
            else putStrLn "popS removió los palets con destino 'CityB' correctamente."
         putStrLn $ "Peso neto después del pop (esperado 9): " ++ show (netS s3p)
         if netS s3p /= (5 + 4)
            then error "popS no actualizó netS correctamente."
            else putStrLn "popS actualizó netS correctamente."

    ----------------------------------------------------------------
    -- TRUCK TESTS
    runTest "Truck: freeCellsT and initial load" $ do
         putStrLn "Creando camión con 2 bahías, altura 2, ruta [CityA, CityB, CityC]..."
         let truckRoute = newR ["CityA", "CityB", "CityC"]
         let truck = newT 2 2 truckRoute  -- 2 bahías con capacidad 2 cada una = 4 celdas
         putStrLn $ "Celdas libres iniciales en el camión (esperado 4): " ++ show (freeCellsT truck)
         if freeCellsT truck /= 4
            then error "freeCellsT incorrecto para camión nuevo."
            else putStrLn "freeCellsT verificado correctamente para camión nuevo."
         putStrLn "Cargando Palet destino 'CityA' peso 3..."
         let t1 = loadT truck (newP "CityA" 3)
         putStrLn $ "Celdas libres tras la primera carga (esperado 3): " ++ show (freeCellsT t1)
         if freeCellsT t1 /= 3
            then error "freeCellsT incorrecto tras cargar un palet."
            else putStrLn "freeCellsT verificado correctamente tras la primera carga."
         putStrLn "Cargando Palet destino 'CityB' peso 4..."
         let t2 = loadT t1 (newP "CityB" 4)
         putStrLn $ "Peso neto del camión (esperado 7): " ++ show (netT t2)
         if netT t2 /= 7
            then error "netT incorrecto tras dos cargas."
            else putStrLn "netT verificado correctamente tras dos cargas."

    runTest "Truck: invalid ordering with single bay" $ do
         putStrLn "Creando camión con 1 bahía, altura 2, ruta [CityA, CityB, CityC]..."
         let truckRoute = newR ["CityA", "CityB", "CityC"]
         let truckSingle = newT 1 2 truckRoute
         putStrLn "Cargando Palet destino 'CityA' peso 3..."
         let t1 = loadT truckSingle (newP "CityA" 3)
         putStrLn "Cargando Palet destino 'CityB' peso 4..."
         let t2 = loadT t1 (newP "CityB" 4)
         putStrLn "Intentando cargar Palet destino 'CityA' peso 2 sobre palet de 'CityB'..."
         if not (testF (loadT t2 (newP "CityA" 2)))
            then error "loadT debería fallar por orden inválido (no se puede cargar CityA después de CityB)."
            else putStrLn "loadT falló correctamente cuando el orden era inválido (CityB->CityA)."

    runTest "Truck: truck capacity" $ do
         putStrLn "Creando camión con 2 bahías, altura 2, ruta [CityA, CityB, CityC]..."
         let truckRoute = newR ["CityA", "CityB", "CityC"]
         let truck = newT 2 2 truckRoute
         putStrLn "Cargando cuatro palets hasta llenar las 4 celdas..."
         
         let t1 = loadT truck (newP "CityC" 3)
         let t2 = loadT t1 (newP "CityB" 4)
         let t3 = loadT t2 (newP "CityA" 2)
         let t4 = loadT t3 (newP "CityA" 1)
         putStrLn $ "Celdas libres tras cargar 4 palets (esperado 0): " ++ show (freeCellsT t4)
         if freeCellsT t4 /= 0
            then error "El camión no está lleno después de 4 cargas."
            else putStrLn "El camión está lleno, como se esperaba."
         putStrLn "Intentando cargar un palet adicional..."
         if not (testF (loadT t4 (newP "CityA" 1)))
            then error "loadT debería fallar cuando el camión está lleno."
            else putStrLn "loadT falló correctamente al no tener más espacio."

    runTest "Truck: unloadT removes palets" $ do
         putStrLn "Creando camión con 2 bahías, altura 3, ruta [CityA, CityB, CityC]..."
         let truckRoute = newR ["CityA", "CityB", "CityC"]
         let truck2 = newT 2 3 truckRoute
         putStrLn "Cargando Palet destino 'CityA' peso 2..."
         let truck2a = loadT truck2 (newP "CityA" 2)
         putStrLn "Cargando Palet destino 'CityB' peso 3..."
         let truck2b = loadT truck2a (newP "CityB" 3)
         putStrLn "Cargando Palet destino 'CityA' peso 4..."
         let truck2c = loadT truck2b (newP "CityA" 4)
         putStrLn $ "Peso neto antes de descargar (esperado 9): " ++ show (netT truck2c)
         putStrLn "Descargando palets con destino 'CityA'..."
         let truck2u = unloadT truck2c "CityA"
         putStrLn $ "Peso neto después de descargar (esperado 3): " ++ show (netT truck2u)
         if netT truck2u /= 3
            then error "unloadT no descargó los palets adecuadamente (netT no coincide)."
            else putStrLn "unloadT descargó los palets de 'CityA' correctamente."

    putStrLn "\n¡Todos los tests se han ejecutado!"
