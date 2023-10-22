module Oraculo (
    Oraculo(..),
    Opciones,
    crearOraculo,
    ramificar,
    prediccion,
    pregunta,
    respuesta,
    opciones,
    maybeOraculo
    ) where


import qualified Data.Map as M

data Oraculo = Prediccion String
    | Pregunta String Opciones

type Opciones = M.Map String Oraculo

-- Crear un nuevo oraculo
crearOraculo :: String -> Oraculo
crearOraculo pred = Prediccion pred

-- Ramificar
ramificar:: [String] -> [Oraculo] -> String -> Oraculo
ramificar ops oraculos preg = Pregunta preg (M.fromList (zip ops oraculos))

-- Obtiene la prediccion
prediccion :: Oraculo -> String
prediccion (Prediccion s ) = s 
prediccion (Pregunta s a) = error "Debe ingresar un prediccion"

-- Obtiene la pregunta 
pregunta :: Oraculo -> String
pregunta (Pregunta s op) = s
pregunta (Prediccion s) = error "Debe ingresar una pregunta"

-- Obtiene las opciones asociadas a una pregunta
opciones ::  Oraculo -> Opciones
opciones (Pregunta s op) = op
opciones (Prediccion s) = error "Debe ingresar una pregunta"

-- Funcion para hallar el oraculo de un maybe oraculo
maybeOraculo :: Maybe Oraculo -> Oraculo
maybeOraculo (Just a) = a
maybeOraculo Nothing = error "No es una pregunta valida"

-- Obtiene el oraculo respuesta asociado a una pregunta y una respuesta dada
respuesta :: Oraculo -> String -> Oraculo
respuesta (Pregunta s op) preg = maybeOraculo(M.lookup preg op)
respuesta (Prediccion s) _ = error "Debe ingresar una pregunta"
