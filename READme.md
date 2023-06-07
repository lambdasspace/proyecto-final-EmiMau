## Proyecto Final: Modelo del Cálculo Lambda

### Especificación:
La intención del proyecto es generar un modelo del Cálculo lambda que nos permita poner abstracciones, aplicaciones y variables utilizando una sintaxis muy similar a aquella que utiliza esta forma teórica de cómputo. La idea es que si funciona en sus formas más básicas después sea posible expandirlo a las variantes existente del cálculo, como son aquellas que actúan con tipado o incluso variables de tipo. Esto se daría si no termina siendo más complicado de lo originalmente planeado.

La sintaxis que tengo planeado utilizar es:
```
(\\x.x) (\\x.x) <- para aplicaciones
\\x.x           <- para abstracciones lambda
x               <- para variables
```

Las variables sólo pueden ser una única letra, de manera que si tenemos más de una letra el parser se encargará de solamente procesar el caracter inicial de aquella sucesión que hubera elegido como identificador.

La idea es que el proyecto permita la aplicación sucesiva de funciones utilizando una estructura de datos similar a una lista, la cual revisa si tiene cola después de hacer cada una de las aplicaciones.
