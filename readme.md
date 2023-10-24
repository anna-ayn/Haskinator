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

Si la opción es inválida, muestra error y vuelve a interactuar.
`interactuar` usa recursión para permitir múltiples interacciones en un loop.

### **obtenerCadena**

La función `obtenerCadena` recibe un oráculo (tipo Oraculo) y una cadena de texto correspondiente a una predicción. Devuelve un valor de tipo Maybe [(String, String)]. \
La implementación de `ObtenerCadena` utiliza una función auxiliar `buscar` para recorrer recursivamente la estructura del oráculo y construir la lista de tuplas (pregunta, opción) que corresponden al camino hacia la predicción dada. Además, para llamar a la función auxiliar `buscar` se pasa el oráculo, la predicción y una lista vacía para el camino. Luego, lo que hace `buscar` es que:

- Caso base: si recibe una Prediccion, verifica si coincide con la predicción buscada.
  - Si coincide, devuelve Just el camino.
  - Si no coincide, devuelve una lista vacía [].
- Si recibe una Pregunta, busca en las opciones a ver si alguna coincide con la predicción.
  - Si encuentra coincidencia, llama recursivamente a `buscar` sobre ese valor de predicción, agregando la pregunta actual y la opción elegida al camino.
  - Si no encuentra, llama recursivamente a buscar sobre cada opción, concatenando los resultados.

Luego te terminar de ejecutar `buscar`, se hace pattern matching sobre el resultado:

- Si es Nothing, devuelve Nothing.
- Si es Just camino, devuelve esto mismo.
