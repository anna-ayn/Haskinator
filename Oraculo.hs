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
    obtenerCadena,
    obtenerEstadisticas
    ) where

-- Importanciones de modulos
import qualified Data.Map as M
import Data.List as D

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

--Funcion que transforma un Integer en un Double para llamar a la funcion que calcula el promedio 
intToDouble :: Integer -> Double
intToDouble n = fromIntegral n

--Funcion que calcula el promedio de una lista de Doubles
promedioCalc :: [Double] -> Double
promedioCalc n = sum(n)/D.genericLength(n)

--Funcion que remueve los 0 de una lista de Integers
removeCero :: [Integer] -> [Integer]
removeCero [] = []
removeCero (h:t)
    | h == 0 = removeCero t
    | h > 0 = h : removeCero t

--Funcion que remueve las Predicciones de una lista de Oraculos
removePred :: [Oraculo] -> [Oraculo]
removePred [] = []
removePred (h:t)
    | esPrediccion(h) = removePred t
    | otherwise = h : removePred t

--Funcion que recibe una lista de Oraculos y expande las Opciones de las Preguntas como una lista de oraculos para que puedan pasar por calcularEstadistica. 
divOra ::  [Oraculo] -> [Oraculo]
divOra [] = []
divOra (h:t) 
    | esPrediccion(h) = h : divOra t
    | otherwise = M.elems(op) ++ divOra t
        where 
        op = opciones h

--Funcion que recibe una lista de Oraculos y un Integer que comineza en 1, con esto llama a calcularEstadistica con una lista expandida devulve una concatencación de esos resultados
--Luego, le aumneta uno al contador de preguntas y elimina las predicciones de la lista de Oraculos, para volver a llamar a divOra
auxEstadistica :: [Oraculo] -> Integer -> [Integer]
auxEstadistica [] num = []
auxEstadistica m num = calcularEstadistica listaExpandida num ++ auxEstadistica listaSinPredicciones i
    where 
    i = num + 1
    listaExpandida = divOra m
    listaSinPredicciones = removePred listaExpandida

-Funcion que recibe una lista de Oraculos y un Integer (este Integer representa el contador de preguntas en ese momento). Agrega en una lista de Integer 0, si el Oraculo es una Pregunta, o i, si el oraculo es una Prediccion.
calcularEstadistica :: [Oraculo] -> Integer -> [Integer]
calcularEstadistica [] i = return [] i
calcularEstadistica (h:t) i 
    | esPrediccion(h) = return i ++ calcularEstadistica t i
    |otherwise =  do
       return 0 ++ calcularEstadistica t i

--Funcion para redondear un a un numero especifico de decimales
roundTo :: Integer -> Double -> Double
roundTo n x = (fromInteger $ round $ x * (10^n)) / (10.0^^n)

--A partir de un Oraculo de pregunta, devuelve el maxino, minimo y el promedio del numero de preguntas para llegar a una predicción
obtenerEstadisticas :: Oraculo -> (Integer, Integer, Double)
--Devuelve la tuplo con solo ceros si se le pasa una prediccion
obtenerEstadisticas (Prediccion s) = (0,0,0.0)
--De lo contrario, llama a auxEstadistica, luego elimina los ceros innecesarios y calcula lo necesario para devolver lo pedido
obtenerEstadisticas (Pregunta s op) =  (minimum listaEstadisticaSinCeros, maximum listaEstadisticaSinCeros, promedio)
    where 
    listaEstadistaCompleta = auxEstadistica [(Pregunta s op)] 1
    listaEstadisticaSinCeros = removeCero listaEstadistaCompleta
    promedio = roundTo 5 (promedioCalc (map intToDouble listaEstadisticaSinCeros)) 

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
