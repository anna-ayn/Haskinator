module Main (
    main
) where

import Oraculo

main = do
    putStrLn "# ¡Bienvenido valiente viajero al gran oráculo Haskinator!"
    putStrLn "# Has logrado llegar hasta la apartada choza del poderoso Haskinator,"
    putStrLn "# oculta en lo profundo del bosque de los mil y un monads," 
    putStrLn "# a la vera del gran río Curry."
    putStrLn "# El destino te ha traído ante la presencia de su insondable sabiduría."
    putStrLn "# Con su vasto conocimiento, Haskinator podrá adivinar tus pensamientos"
    putStrLn "# haciéndote solo unas pocas y puntuales preguntas."

    interactuar $ crearOraculo ""

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
        "1" -> crearVision

        "2" -> do
            nuevOraculo <- predecir oraculo
            interactuar nuevOraculo

        "7" -> do
            putStrLn "♦ ¡Hasta luego, viajero! Vuelve pronto."
            return ()
            
        _ -> do
<<<<<<< HEAD
            putStrLn "\n♦ Opción inválida. Vuelve a introducirla"
=======
            putStrLn "\nOpción inválida"
>>>>>>> 311a3b24bc160be8c8412b88f94375644653a5f8
            interactuar oraculo

crearVision :: IO ()
crearVision = do 
    putStrLn "\n♦ Cuenta al gran Haskinator la visión que deseas incorporar a su conocimiento:"
    prediccion <- getLine
    let nuevoOraculo = crearOraculo prediccion
    putStrLn "\n♦ La visión ha sido agregada al oráculo."
    interactuar nuevoOraculo


predecir :: Oraculo -> IO Oraculo
predecir oraculo = do
    if esPrediccion oraculo
        then do
            putStrLn "Es una prediccion..."
            let vision = obtenerPred oraculo
            if vision == "" then do
                putStrLn "\n♦ El oráculo no posee conocimientos aún."
                putStrLn "Por favor crea un nuevo oráculo o cargue la información al oráculo antes de predecir."
                return oraculo
            else do
                proponerPred oraculo
    else do
        putStrLn "Es una pregunta..."
        return oraculo

proponerPred :: Oraculo -> IO Oraculo
proponerPred oraculo = do
    let vision = obtenerPred oraculo
    putStrLn ("\nPredicción: " ++ vision)
    putStrLn "Si / No"

    input <- getLine

    case input of
      "Si" -> do 
        putStrLn "\n♦ ¡He hecho la predicción correcta!"
        return oraculo
      "No" -> do 
        putStrLn "\n♦ ¡He fallado! ¿Cuál es la respuesta correcta?"
        rtaCorrecta <- getLine

        putStrLn $ "♦ ¿Qué pregunta distingue a " ++ rtaCorrecta ++ " de las otras opciones?"
        pregDiferencia <- getLine

        putStrLn $ "♦ ¿Cuál es la respuesta a '" ++ pregDiferencia ++ "' para " ++ rtaCorrecta ++"?"
        opCorrecta <- getLine

        putStrLn $ "♦ ¿Cuál es la respuesta a '" ++ pregDiferencia ++ "' para " ++ vision ++"?"
        otraOpcion <- getLine

        return (ramificar [opCorrecta, otraOpcion] [crearOraculo rtaCorrecta, oraculo] pregDiferencia)
      _ -> do 
        putStrLn "\n♦ ¡Opción inválida. Vuelve a introducirla."
        proponerPred oraculo

        



    -- let miPrediccion = Prediccion "Haskell es un lenguaje funcional"
    --     otraPrediccion = Prediccion "Haskell es un lenguaje imperativo"
    --     miPregunta = Pregunta "¿Es un lenguaje funcional?" opciones 
    --         where opciones = M.fromList [ ("Si", miPrediccion), ("No", otraPrediccion) ]
    
    -- let oraculo = crearOraculo "Este es mi prediccion"

    -- let myMap = M.fromList [(1,"hello"), (3,"goodbye")]
    -- -- find myMap[3]
    -- case M.lookup 3 myMap of
    --     Just val -> print val
    --     Nothing -> putStrLn "Key not found"
    
