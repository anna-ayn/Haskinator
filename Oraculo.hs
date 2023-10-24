module Oraculo (
    Oraculo(..),
    Opciones,
    crearOraculo,
    ramificar,
    prediccion,
    pregunta,
    respuesta,
    opciones,
    maybeOraculo,
    obtenerCadena
    ) where

-- Importanciones de modulos
import qualified Data.Map as M
import Data.List (find)

-- Oraculo
data Oraculo = Prediccion String
    | Pregunta String Opciones
    deriving(Show,Read)
    
-- Opciones
type Opciones = M.Map String Oraculo

-- Comparar dos oraculos si son iguales o no
instance Eq Oraculo where
  (Prediccion s1) == (Prediccion s2) = s1 == s2
  (Pregunta s1 o1) == (Pregunta s2 o2) = s1 == s2 && o1 == o2
  _ == _ = False

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

-- Obtiene el oraculo respuesta asociado a una pregunta y una respuesta dada
respuesta :: Oraculo -> String -> Oraculo
respuesta (Pregunta s op) preg = maybeOraculo(M.lookup preg op)
respuesta (Prediccion s) _ = error "Debe ingresar una pregunta"

-- Funcion para hallar el oraculo de un maybe oraculo
maybeOraculo :: Maybe Oraculo -> Oraculo
maybeOraculo (Just a) = a
maybeOraculo Nothing = error "No es una pregunta valida"

-- La funcion obtenerCadena recibe un oraculo y una cadena de caracteres correspondiente a una prediccion. 
-- Devuelve un valor de tipo Maybe [(String, String)]. 
-- Si la prediccion suministrado no pertenece al oraculo, se retorna Nothing. 
-- De lo contrario retorna Just lista, donde lista es una lista de tuplas (de dos cadenas de caracteres) 
-- que corresponden a todas las preguntas que deben hacerse, a partir de la raiz del oraculo, 
-- para alcanzar la prediccion suministrada y el valor de la opcion escogida. 
obtenerCadena :: Oraculo -> String -> Maybe [(String, String)]
obtenerCadena oraculo pred = do
  -- usaremos una funcion auxiliar para crear la lista de tuplas 
  let result = buscar oraculo pred []
  case result of
    Just [] -> Nothing
    _ -> result

-- La funcion buscar recibe un oraculo, una cadena de texto y una lista de tuplas (String, String). Esta lista
-- va a utilizarse para construir el camino a traves del oraculo que conduce a la prediccion dada. 
-- Si encuentra un camino hacia la prediccion, retorna Just camino
-- En caso contrario retorna una lista vacia = [] 
buscar (Prediccion p) pred camino = do
  -- Si buscar recibe una prediccion, se verifica que sea igual a la prediccion que se busca 
  -- si son iguales, retorna Just camino, en caso contrario retorna una lista vacia = []
  if p == pred then Just camino else return []
buscar (Pregunta preg ops) pred camino = do
  -- Si buscar recibe una pregunta
  let value = Prediccion pred
  -- Buscamos si existe una opcion de la pregunta tal que este asociada a la prediccion buscada
  case fmap fst $ find ((== value) . snd) (M.toList ops) of
      -- si la prediccion buscada esta asociada a una de las opciones, entonces agrego a la lista camino 
      -- la tupla con la pregunta y la opcion
      Just key -> buscar value pred (camino ++ [(preg, key)])
      Nothing -> do
        -- en caso contrario, navego en los subOraculos
        result <- mapM (\(opcion', val) -> do
          buscar val pred (camino ++ [(preg, opcion')])) (M.toList ops)
        return $ concat result
