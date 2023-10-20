module Main (
    main
) where

import Oraculo

main = do
    putStrLn "¡Bienvenido valiente viajero al gran oráculo Haskinator!"
    putStrLn "Has logrado llegar hasta la apartada choza del poderoso Haskinator,"
    putStrLn "oculta en lo profundo del bosque de los mil y un monads," 
    putStrLn "a la vera del gran río Curry."
    putStrLn "El destino te ha traído ante la presencia de su insondable sabiduría."
    putStrLn "Con su vasto conocimiento, Haskinator podrá adivinar tus pensamientos"
    putStrLn "haciéndote solo unas pocas y puntuales preguntas."

    interactuar $ crearOraculo ""

interactuar :: Oraculo -> IO ()
interactuar oraculo = do 
        
    putStrLn "\n¿Qué deseas hacer ahora ante el poderoso Haskinator?\n"

    putStrLn "Opciones:"
    putStrLn "1. Crear Oraculo"
    putStrLn "2. Predecir"
    putStrLn "3. Persistir"
    putStrLn "4. Cargar"
    putStrLn "5. Consultar pregunta crucial"
    putStrLn "6. Estadísticas"
    putStrLn "7. Salir\n"

    putStrLn "Ingrese solo el número de la opción:"
    opcion <- getLine

    case opcion of 
        "1" -> crearVision

        "2" -> do
            putStrLn "Codigo Para Predecir..."
            interactuar oraculo 

        "7" -> do
            putStrLn "¡Hasta luego, viajero! Vuelve pronto."
            return ()
            
        _ -> do
            putStrLn "\nOpción inválida"
            interactuar oraculo

crearVision :: IO ()
crearVision = do 
    putStrLn "\nCuenta al gran Haskinator la visión que deseas incorporar a su conocimiento:"
    prediccion <- getLine
    let nuevoOraculo = crearOraculo prediccion
    putStrLn "\nLa visión ha sido agregada al oráculo."
    interactuar nuevoOraculo





        



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
    