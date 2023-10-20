module Oraculo (
    Oraculo,
    Opciones,
    crearOraculo,
    ramificar
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