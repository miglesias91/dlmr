# dlmr
### Paquete R para hacer consultas a la base de [dlm](https://twitter.com/dicenlosmedios).

Wrapper sobre [mongolite](https://github.com/jeroen/mongolite) para **simplificar** las consultas.

---
# ***Instalación***

0. Instalar [devtools](https://github.com/r-lib/devtools). Desde consola:
```
> install.packages("devtools")
```
1. Instalar [dlmr](https://github.com/miglesias91/dlmr). Desde consola:
 ```
> devtools::install_github("miglesias91/dlmr")
```
# ***Uso***
***ACLARACIÓN:*** *Para hacer consultas a la base de datos de [dicenlosmedios](https://twitter.com/dicenlosmedios), hace falta tener **usuario**, **contraseña** e **ip del servidor**.*

## 1. Abrir conexión:
Antes que nada hay que conectarse a la base de datos:
```
> conectar(usuario = 'un_usuario', password = 'una_password', servidor = 'una_ip')
```
Este método se llama **una sola vez por sesión** (*cada vez que se abre [**RStudio**](https://rstudio.com/)*).

## 2. Consultar
### 2.1. *Frecuencias*
Esta consulta devuelve las *veces que se mencióno* -*'frecuencia'*- una palabra en una determinada fecha. 
```
> freqs = frecuencia(que,
                     donde,
                     palabras,
                     desde = '0',
                     hasta = '99999999',
                     diarios = c(),
                     categorias = c(),
                     freq_min = 0,
                     freq_max = 99999)
```
Descripción de paramétros:

- ***que***: "qué tipo de palabras buscar?" este paramétros puede ser '*personas*', '*terminos*' o '*verbos*'.
- ***donde***: "dónde buscar las palabras?" este paramétros puede ser '*titulos*' o '*textos*'.
- ***palabras***: "cuáles palabras buscar?" acá van las palabras que se quieren buscar. Apellido de una *persona*, o *término*, o verbo en *infinitivo*.
- ***desde***: "desde qué fecha buscar?" fecha desde la cual se quiere recuperar las frecuencias, en formato 'YYYYMMDD'.
- ***hasta***: "hasta qué fecha buscar?" fecha hastas la cual se quiere recuperar las frecuencias, en formato 'YYYYMMDD'.
- 
