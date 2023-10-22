module Main (
    main
) where

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
            nuevOraculo <- predecir oraculo
            interactuar nuevOraculo
        "7" -> do -- salir de haskinator
            putStrLn "\n♦¡Hasta luego, viajero! Vuelve pronto."
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
predecir :: Oraculo -> IO Oraculo
predecir oraculo = do
    if esPrediccion oraculo -- si es una prediccion
        then do
            -- verificar que la prediccion no este vacia
            let vision = prediccion oraculo
            if vision == "" then do 
                putStrLn "\n♦ El oráculo no posee conocimientos aún."
                putStrLn "Por favor crea un nuevo oráculo o cargue la información al oráculo antes de predecir."
                return oraculo
            else do -- si no esta vacia, se le propone la prediccion
                proponerPred oraculo
    else proponerPreg oraculo -- si es una pregunta, se le propone la pregunta

-- Funcion que retorna True si el Oraculo es una Prediccion en caso contrario retorna False
esPrediccion :: Oraculo -> Bool
esPrediccion (Prediccion _) = True
esPrediccion _ = False

-- Proponer al usuario la prediccion
proponerPred :: Oraculo -> IO Oraculo
proponerPred oraculo = do
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
        rtaCorrecta <- getLine
        -- verificar que la respuesta correcta sea unica
        if rtaCorrecta == vision then do 
            putStrLn $ "\n♦ Ya existe la predicción '" ++ rtaCorrecta ++ "' en el oráculo."
            putStrLn "Se va a rechazar la adición de esta predicción al oráculo por ser poco confiable."
            return oraculo
        else do -- si es unica, se le pide al usuario las siguientes preguntas:
            putStrLn $ "♦ ¿Qué pregunta distingue a " ++ rtaCorrecta ++ " de las otras opciones?"
            pregDiferencia <- getLine
            putStrLn $ "♦ ¿Cuál es la respuesta a '" ++ pregDiferencia ++ "' para " ++ rtaCorrecta ++"?"
            opCorrecta <- getLine
            putStrLn $ "♦ ¿Cuál es la respuesta a '" ++ pregDiferencia ++ "' para " ++ vision ++"?"
            otraOpcion <- getLine
            -- se agrega esta nueva informacion al oraculo
            return (ramificar [otraOpcion, opCorrecta] [oraculo, crearOraculo rtaCorrecta] pregDiferencia)
      _ -> do -- opcion invalida
        printOpInvalida
        proponerPred oraculo

-- Proponer la pregunta al usuario
proponerPreg :: Oraculo -> IO Oraculo
proponerPreg oraculo = do
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
            putStrLn preg
            nuevaOpcion <- getLine
            -- se agrega al oraculo esta nueva opcion
            return (insertarOpcion oraculo nuevaOpcion (crearOraculo nuevaRespuesta) )
        _ -> do -- verificar si el input que puso el usuario es una de las opciones validas
            case M.lookup input (opciones oraculo) of
                Just val -> do
                    -- si la opcion es valida, se navega al sub-oraculo correspondiente
                    siguiente <- predecir (respuesta oraculo input)
                    -- agrego los cambios del oraculo al oraculo original
                    return (insertarOpcion oraculo input siguiente)
                Nothing -> do
                    -- la opcion es invalida
                    printOpInvalida 
                    putStrLn "Si no se encuentra la opción que deseas escoger, escribe: ninguna"
                    proponerPreg oraculo

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
    putStrLn "\n♦ El gran Haskinator no comprende tu elección." 
    putStrLn "Por favor, selecciona una de las opciones que se te ofrece."