module Main (
    main
) where

import System.IO
import Control.Exception
import Oraculo
import qualified Data.Map as M
import Data.List (intercalate)

main = do
    putStrLn "# ¡Bienvenido valiente viajero al gran oráculo Haskinator!"
    putStrLn "# Has logrado llegar hasta la apartada choza del poderoso Haskinator,"
    putStrLn "# oculta en lo profundo del bosque de los mil y un monads," 
    putStrLn "# a la vera del gran río Curry."
    putStrLn "# El destino te ha traído ante la presencia de su insondable sabiduría."
    putStrLn "# Con su vasto conocimiento, Haskinator podrá adivinar tus pensamientos"
    putStrLn "# haciéndote solo unas pocas y puntuales preguntas."

    interactuar $ crearOraculo ""

-- Se pide al usuario repetidamente que ingrese alguna de las opciones y luego pasa a ejecutarla
interactuar :: Oraculo -> IO ()
interactuar oraculo = do 
        
    putStrLn "\n♦ ¿Qué deseas hacer ahora ante el poderoso Haskinator?\n"
    putStrLn " -------------------------------------- "
    putStrLn "|Opciones:                             |"
    putStrLn "|1. Crear Oraculo                      |"
    putStrLn "|2. Predecir                           |"
    putStrLn "|3. Persistir                          |"
    putStrLn "|4. Cargar                             |"
    putStrLn "|5. Consultar pregunta crucial         |"
    putStrLn "|6. Estadísticas                       |"
    putStrLn "|7. Salir                              |"
    putStrLn " -------------------------------------- "

    putStrLn "Ingrese solo el número de la opción:"
    opcion <- getLine

    case opcion of 
        "1" -> crearVision -- crear nuevo oraculo
        "2" -> do -- predecir
            let oracOriginal = oraculo
            nuevOraculo <- predecir oraculo oracOriginal
            interactuar nuevOraculo
        "3" -> do 
            persistir oraculo
            interactuar oraculo
        "4" -> do
            putStrLn "\n♦ ¿Cuál es el archivo que deseas mostrar al gran Haskinator?"
            nombreArchivo <- getLine
            oraculo <- cargar nombreArchivo
            interactuar oraculo
        "7" -> do -- salir de haskinator
            putStrLn "\n♦ ¡Hasta luego, viajero! Vuelve pronto."
            return ()
        _ -> do -- opcion invalida
            printOpInvalida 
            interactuar oraculo

-- Crear un nuevo oraculo: se le pide al usuario una prediccion
-- y se almacena como la unica prediccion del oraculo.
crearVision :: IO ()
crearVision = do 
    putStrLn "\n♦ Cuenta al gran Haskinator la visión que deseas incorporar a su conocimiento:"
    predic <- getLine
    let nuevoOraculo = crearOraculo predic
    putStrLn "\n♦ La visión ha sido agregada al oráculo."
    interactuar nuevoOraculo

-- Predecir: Se comienza el proceso de prediccion
predecir :: Oraculo -> Oraculo -> IO Oraculo
predecir oraculo oracOriginal = do
    if esPrediccion oraculo -- si es una prediccion
        then do
            -- verificar que la prediccion no este vacia
            let vision = prediccion oraculo
            if vision == "" then do 
                putStrLn "\n♦ El oráculo no posee conocimientos aún."
                putStrLn "+ Por favor crea un nuevo oráculo o cargue la información al oráculo antes de predecir."
                return oraculo
            else do -- si no esta vacia, se le propone la prediccion
                proponerPred oraculo oracOriginal
    else proponerPreg oraculo oracOriginal -- si es una pregunta, se le propone la pregunta

-- Funcion que retorna True si el Oraculo es una Prediccion en caso contrario retorna False
esPrediccion :: Oraculo -> Bool
esPrediccion (Prediccion _) = True
esPrediccion _ = False

-- Proponer al usuario la prediccion
proponerPred :: Oraculo -> Oraculo -> IO Oraculo
proponerPred oraculo oracOriginal = do
    let vision = prediccion oraculo

    -- El usuario decide si la prediccion es acertada o no
    putStrLn ("\nPredicción: " ++ vision)
    putStrLn "Si / No"

    input <- getLine

    case input of
      "Si" -> do -- la prediccion es acertada, se termina la accion predecir
        putStrLn "\n♦ ¡He hecho la predicción correcta!"
        return oraculo
      "No" -> do -- en caso contrario, debe pedir al usuario la respuesta correcta
        putStrLn "\n♦ ¡He fallado! ¿Cuál es la respuesta correcta?"
        predCorrecta <- getLine
        -- verificar que la prediccion correcta sea unica
        if (length $ obtenerCadena oracOriginal predCorrecta) > 0 || predCorrecta == vision then do 
            printPredYaExiste predCorrecta
            return oraculo
        else do -- si es unica, se le pide al usuario las siguientes preguntas:
            putStrLn $ "♦ ¿Qué pregunta distingue a " ++ predCorrecta ++ " de las otras opciones?"
            pregDiferencia <- getLine
            putStrLn $ "♦ ¿Cuál es la respuesta a '" ++ pregDiferencia ++ "' para " ++ predCorrecta ++"?"
            opCorrecta <- getLine
            putStrLn $ "♦ ¿Cuál es la respuesta a '" ++ pregDiferencia ++ "' para " ++ vision ++"?"
            otraOpcion <- getLine
            -- se agrega esta nueva informacion al oraculo
            return (ramificar [otraOpcion, opCorrecta] [oraculo, crearOraculo predCorrecta] pregDiferencia)
      _ -> do -- opcion invalida
        printOpInvalida
        proponerPred oraculo oracOriginal

-- Proponer la pregunta al usuario
proponerPreg :: Oraculo -> Oraculo -> IO Oraculo
proponerPreg oraculo oracOriginal = do
    let preg = pregunta oraculo
    putStrLn ("\n" ++ preg)
    -- se imprime las opciones a esta pregunta, el usuario debe escoger una de ellas.
    printOp $ opciones oraculo

    input <- getLine

    case input of 
        -- si el usuario introduce ninguna, hay que pedirle la respuesta correcta y la opcion asociada a la respuesta
        "ninguna" -> do
            putStrLn "\n♦ ¡He fallado! ¿Cuál es la respuesta correcta?"
            nuevaRespuesta <- getLine
            if (length $ obtenerCadena oracOriginal nuevaRespuesta) > 0 then do
                printPredYaExiste nuevaRespuesta
                return oraculo
            else do
                putStrLn preg
                nuevaOpcion <- getLine
                -- se agrega al oraculo esta nueva opcion
                return (insertarOpcion oraculo nuevaOpcion (crearOraculo nuevaRespuesta) )
        _ -> do -- verificar si el input que puso el usuario es una de las opciones validas
            case M.lookup input (opciones oraculo) of
                Just val -> do
                    -- si la opcion es valida, se navega al sub-oraculo correspondiente
                    siguiente <- predecir (respuesta oraculo input) oracOriginal
                    -- agrego los cambios del oraculo al oraculo original
                    return (insertarOpcion oraculo input siguiente)
                Nothing -> do
                    -- la opcion es invalida
                    printOpInvalida 
                    putStrLn "+ Si no se encuentra la opción que deseas escoger, escribe: ninguna"
                    proponerPreg oraculo oracOriginal

-- Funcion que imprime las opciones de una pregunta, separados por /
printOp :: Opciones -> IO ()
printOp elecciones = do
  let keys = M.keys elecciones
  putStrLn $ intercalate " / " keys

-- Funcion para agregar una nueva opcion y una nueva respuesta al oraculo
insertarOpcion :: Oraculo -> String -> Oraculo -> Oraculo 
insertarOpcion (Pregunta preg ops) nuevaOpcion nuevaRespuesta = 
    let nuevasOpciones = M.insert nuevaOpcion nuevaRespuesta ops
    in Pregunta preg nuevasOpciones

-- Funcion que imprime un texto que indica que la opcion que introdujo el usuario es invalida
printOpInvalida :: IO  ()
printOpInvalida = do
    putStrLn "\n⚠ El gran Haskinator no comprende tu elección." 
    putStrLn "+ Por favor, selecciona una de las opciones que se te ofrece."

-- Funcion que imprime un texto que indica que ya existe la prediccion en el oraculo y por lo tanto
-- no se va a agregar al oraculo
printPredYaExiste :: String -> IO ()
printPredYaExiste predRepetida = do 
    putStrLn $ "\n⚠ Ya existe la predicción '" ++ predRepetida ++ "' en el oráculo."
    putStrLn "+ Se va a rechazar la adición de esta predicción al oráculo por ser poco confiable."

-- Funcion que guarda un oraculo en un archivo
persistir :: Oraculo -> IO ()
persistir oraculo = do
    putStrLn "\n♦ ¿Cómo deseas llamar al archivo?"
    nombre <- getLine
    let contents = show oraculo
    -- Se intenta escribir en el archivo
    result <- try (writeFile nombre contents) :: IO (Either SomeException ())
    case result of 
        Left ex -> do
            putStrLn $ "\n⚠ No se pudo escribir en el archivo: '" ++ nombre ++ "'."
            putStrLn "+ Compruebe que el archivo no esté siendo utilizado actualmente."
        Right _ -> putStrLn $ "\n♦ El oráculo ha sido guardado en el archivo '" ++ nombre ++ "'."

-- Funcion que dado el nombre de un archivo, carga el oraculo que se encuentra en el archivo
cargar :: String -> IO Oraculo
cargar nombre = do
    -- se intenta leer el arhivo
    result <- try (readFile nombre) :: IO (Either SomeException String)
    case result of
        -- si se produjo un error al intentar abrir el archivo
        Left ex -> do 
            putStrLn $ "\n⚠ No se pudo abrir el archivo: '" ++ nombre ++ "'."
            putStrLn "+ Compruebe si el archivo existe en la ubicación correcta y si no está siendo utilizado actualmente."
            return $ crearOraculo ""
        -- en caso contrario
        Right contents -> do
            let oraculo = read contents :: Oraculo
            putStrLn $ "\n♦ El oráculo '" ++ nombre ++ "' ha sido cargado."
            return oraculo
