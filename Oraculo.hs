module Oraculo (
    Oraculo,
    Opciones,
    crearOraculo,
    ramificar,
    obtenerPred,
    obtenerPreg,
    obtenerRta,
    obtenerOp,
    maybeOraculo,
    esPrediccion
    ) where


import qualified Data.Map as M

data Oraculo = Prediccion String
    | Pregunta String Opciones

type Opciones = M.Map String Oraculo

-- Crear un nuevo oraculo
crearOraculo :: String -> Oraculo
crearOraculo prediccion = Prediccion prediccion

-- Ramificar
ramificar:: [String] -> [Oraculo] -> String -> Oraculo
ramificar opciones oraculos pregunta = Pregunta pregunta (M.fromList (zip opciones oraculos))

-- Obtiene la prediccion
obtenerPred :: Oraculo -> String
obtenerPred (Prediccion s ) = s 
obtenerPred (Pregunta s a) = error "Debe ingresar un prediccion"

-- Obtiene la pregunta 
obtenerPreg :: Oraculo -> String
obtenerPreg (Pregunta s op) = s
obtenerPreg (Prediccion s) = error "Debe ingresar una pregunta"

-- Obtiene las opciones asociadas a una pregunta
obtenerOp ::  Oraculo -> Opciones
obtenerOp (Pregunta s op) = op
obtenerOp (Prediccion s) = error "Debe ingresar una pregunta"

-- Funcion para hallar el oraculo de un maybe oraculo
maybeOraculo :: Maybe Oraculo -> Oraculo
maybeOraculo (Just a) = a
maybeOraculo Nothing = error "No es una pregunta valida"

-- Obtiene el oraculo respuesta asociado a una pregunta y una respuesta dada
obtenerRta :: Oraculo -> String -> Oraculo
obtenerRta (Pregunta s op) preg = maybeOraculo(M.lookup preg op)
obtenerRta (Prediccion s) _ = error "Debe ingresar una pregunta"

-- Si el Oraculo es una Prediccion, retorna True en caso contrario retorna False
esPrediccion :: Oraculo -> Bool
esPrediccion (Prediccion _) = True
esPrediccion _ = False