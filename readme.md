# HASKINATOR

### Ana Shek - 1910096

### Andrea Diaz - 1810826

### Jeamhowards Montiel - 1910234

## Como ejecutar el programa

1. Escribe en tu consola la palabra

```
make
```

2. Seguidamente

```
./haskinator
```

3. **(Opcional)** Si quieres borrar todos los archivos que has compilado, puedes escribir en la consola

```
make clean
```

- **NOTA** : al ejecutar `make clean`, se va a borrar todos los archivos .hi y .o que se han creado en el paso 1, inclusive el programa ejecutable `haskinator` por lo que el programa no va a existir luego de hacer este paso.

## Detalles importantes de la implementación del oráculo

### **main**

El objetivo del `main` es permitir una interacción por consola con el usuario para adivinar predicciones.
Se implementa lo siguiente:

1. Se imprimen algunos mensajes de bienvenida para presentar el contexto al usuario.
2. Se crea un oráculo vacío llamando a crearOraculo "".
3. Se define una función `interactuar` que se encarga de la interacción principal utilizando el oráculo creado.

### **interactuar**

`interactuar` muestra un menú de opciones y pide al usuario elegir una. Según la opción ingresada, ejecuta una acción diferente:

1. Crear un nuevo oráculo
2. Predecir
3. Persistir
4. Cargar
5. Consultar pregunta crucial
6. Estadísticas.
7. Salir

Si la opción es inválida, muestra error y vuelve a interactuar. \
`interactuar` usa recursión para permitir múltiples interacciones en un loop.

### **predecir**

Inicia el proceso de predicción. Verifica si el oráculo es una predicción o pregunta y llama a `proponerPred` o `proponerPreg` respectivamente. En lo siguiente se detalla las funciones auxiliares que se utilizaron para implementar `predecir`

1. `esPrediccion`: Devuelve true si el oráculo es una Prediccion.
2. `proponerPred`: Propone la predicción al usuario y según su respuesta, termina o pide la predicción correcta. En caso de agregar la predicción correcta al oráculo se verifica que sea única.
3. `proponerPreg`: Propone la pregunta al usuario. Según su respuesta, navega al sub-oráculo o pide una nueva opción/respuesta y la agrega al oráculo.
4. `printOp`: Imprime las opciones de una pregunta separadas por /.
5. `insertarOpcion`: Agrega una nueva opción y respuesta al oráculo correspondiente.
6. `printPredYaExiste`: Imprime un mensaje de predicción repetida y que por lo tanto no se puede agregarlo al oráculo.

También se utilizaron las funciones `obtenerCadena` y `printOpInvalida` que lo detallaremos más adelante.

### **obtenerCadena**

La función `obtenerCadena` recibe un oráculo (tipo Oraculo) y una cadena de texto correspondiente a una predicción. Devuelve un valor de tipo Maybe [(String, String)]. \
La implementación de `ObtenerCadena` es similar al DFS (Depth First Search) para grafos. Asi mismo, esta función utiliza una función auxiliar `buscar` para recorrer recursivamente la estructura del oráculo y construir la lista de tuplas (pregunta, opción) que corresponden al camino hacia la predicción dada. Además, para llamar a la función auxiliar `buscar` se pasa el oráculo, la predicción y una lista vacía para el camino. Luego, lo que hace `buscar` es que:

- Caso base: si recibe una Prediccion, verifica si coincide con la predicción buscada.
  - Si coincide, devuelve Just el camino.
  - Si no coincide, devuelve una lista vacía [].
- Si recibe una Pregunta, busca en las opciones a ver si alguna coincide con la predicción.
  - Si encuentra coincidencia, llama recursivamente a `buscar` sobre ese valor de predicción, agregando la pregunta actual y la opción elegida al camino.
  - Si no encuentra, llama recursivamente a buscar sobre cada opción, concatenando los resultados.

Luego te terminar de ejecutar `buscar`, se hace pattern matching sobre el resultado:

- Si es Nothing, devuelve Nothing.
- Si es Just camino, devuelve esto mismo.

### **printOpInvalida**

Imprime un mensaje de que la opción es inválida en caso de que la opción que introdujo el usuario no coincida con ninguna de las opciones propuestas.


### **persistir**
Pide al usuario que ingrese el nombre del archivo donde se va a guardar el oráculo. Se intenta escribir, si no se puede, se ejecuta una excepción y se imprime un mensaje de error. Si se puede, se escribe el oráculo en el archivo y se imprime un mensaje de éxito.

### **cargar**
Pide al usuario que ingrese el nombre del archivo donde se va a cargar el oráculo. Se intenta leer, si no se puede, se ejecuta una excepción y se imprime un mensaje explicativo y se crea un oraculo vacio. Si se puede, se lee el oráculo del archivo y se retorna dicho oraculo.

### **consultarAncestroComunBajo**
Recibe dos listas de tuplas correspondientes al camino en el arbol del oraculo de dos predicciones. Se combinan las dos listas en una lista de pares de preguntas y opciones mediante `zip`. Se recorre la lista de pares de preguntas y opciones hasta que se encuentre una pregunta que no coincida, en ese momento se retorna la ultima pregunta junto con las opciones de esa pregunta que llevan hacia las predicciones.

### **consultarPreguntaCrucial**
Pide al usuario dos cadenas correspondientes a dos predicciones. Se obtienen los caminos de cada predicción mediante `obtenerCadena`. En caso de existir dichos caminos se llama a `consultarAncestroComunBajo` con los caminos obtenidos. Se imprime la pregunta crucial y las opciones que llevan a cada predicción.