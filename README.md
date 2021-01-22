# dlmr
### Paquete R para hacer consultas a la base de [dlm](https://twitter.com/dicenlosmedios).

Wrapper sobre [mongolite](https://github.com/jeroen/mongolite) para **simplificar** las consultas.

---
# ***Instalación***

0. Instalar [`devtools`](https://github.com/r-lib/devtools). Desde consola:
```
> install.packages("devtools")
```
1. Instalar [`dlmr`](https://github.com/miglesias91/dlmr). Desde consola:
 ```
> devtools::install_github("miglesias91/dlmr")
```
# ***Uso***
***ACLARACIÓN:*** *Para hacer consultas a la base de datos de [`dicenlosmedios`](https://twitter.com/dicenlosmedios), hace falta tener **usuario**, **contraseña** e **ip del servidor**.*

## 1. Abrir conexión
Antes que nada hay que conectarse a la base de datos:
```
> conectar(usuario = 'un_usuario', password = 'una_password', servidor = 'una_ip')
```
Este método se llama **una sola vez por sesión** (*cada vez que se abre [`RStudio`](https://rstudio.com/)*).

## 2. Consultas
### 2.1. *Noticias*
Trae las noticas según los filtros.
```
> notis = noticias(desde,
                   hasta,
                   diarios = c(),
                   categorias = c(),
                   palabras_en_titulo = c(),
                   palabras_en_texto = c())
```
- ***desde***: "*desde qué fecha buscar?*" fecha desde la cual se quiere recuperar las frecuencias, en formato 'YYYYMMDD'.
- ***hasta***: "*hasta qué fecha buscar?*" fecha hasta la cual se quiere recuperar las frecuencias, en formato 'YYYYMMDD'.
- ***diarios***: "*en qué diarios buscar?*" lista de los darios a buscar las palabras.
- ***categorias***: "*en qué categorias buscar?*" lista de las categorias a buscar las palabras.
- ***palabras_en_titulo***: "*cuáles deben aparecer en el título?*" Pueden ser también *pedazos* de palabras. Matchea si aparecen **TODAS** las palabras.
- ***palabras_en_texto***: "*cuáles deben aparecer en el texto?*" Pueden ser también *pedazos* de palabras. Matchea si aparecen **TODAS** las palabras.

### 2.2. *Frecuencias*
Consulta las *veces que se mencióno* -*'frecuencia'*- una palabra en una determinada fecha, para los diarios y categorias indicadas. 
```
> freqs = frecuencia(que,
                     donde,
                     palabras,
                     desde = '18100525',
                     hasta = '20220101',
                     diarios = c(),
                     categorias = c(),
                     freq_min = 0,
                     freq_max = 99999)
```
- ***que***: "*qué tipo de palabras buscar?*" este paramétros puede ser '*personas*', '*terminos*' o '*verbos*'.
- ***donde***: "*dónde buscar las palabras?*" este paramétros puede ser '*titulos*' o '*textos*'.
- ***palabras***: "*cuáles palabras buscar?*" acá van las palabras que se quieren buscar. Apellido de una *persona*, o *término*, o verbo en *infinitivo*.
- ***desde***: "*desde qué fecha buscar?*" fecha desde la cual se quiere recuperar las frecuencias, en formato 'YYYYMMDD'.
- ***hasta***: "*hasta qué fecha buscar?*" fecha hasta la cual se quiere recuperar las frecuencias, en formato 'YYYYMMDD'.
- ***diarios***: "*en qué diarios buscar?*" lista de los darios a buscar las palabras.
- ***categorias***: "*en qué categorias buscar?*" lista de las categorias a buscar las palabras.
- ***freq_min***: "*cuántas veces COMO MÍNIMO tiene que aparecer cada palabra?*"
- ***freq_max***: "*cuántas veces COMO MÁXIMO tiene que aparecer cada palabra?*"

### 2.3. *Tendencias*
Consulta el top N de frecuencias en una determinada fecha, filtrando por diarios y categorias. 
```
> tends = tendencias(que,
                     donde,
                     fecha,
                     diarios = c(),
                     categorias = c(),
                     top = 10)
```
- ***que***: "*qué tipo de palabras buscar?*" este paramétros puede ser '*personas*', '*terminos*' o '*verbos*'.
- ***donde***: "*dónde buscar las palabras?*" este paramétros puede ser '*titulos*' o '*textos*'.
- ***fecha***: "*qué fecha buscar?*" fecha en la cual se quiere recuperar las tendencias, en formato 'YYYYMMDD'.
- ***diarios***: "*en qué diarios buscar?*" lista de los darios a buscar las palabras.
- ***categorias***: "*en qué categorias buscar?*" lista de las categorias a buscar las palabras.

## 3. Info a tener en cuenta
- Todas las consultas devuelve un `data.frame` en formato [`data.table`](https://github.com/Rdatatable/data.table)
- Para buscar **palabras** escribirlas tal como son: usar *mayúsculas*, *minúsculas* y *tildes*. ('reactivación' no es lo mismo que 'reactivacion', **USAR TILDES**)
- En las consulta `noticias()`, podemos buscar pedazos de palabras:
    > "palabras que empiecen con `corrup`" -> noticias(..., palabras_en_texto = c('corrup'))

## 4. Ejemplos
### 4.1.
> "recuperar las **noticias** entre `2020-10-01` y `2020-10-30`, de los darios `Clarin` y `La Nacion`, de las categorías `Economía` y `Política`, que aparezca en el título `'Crisis'`, y en el texto `'conflicto'`, `'reactivación'` y `'corrup'`"
```
> n = noticias(desde = '20201001',
               hasta = '20201030',
               diarios = c('clarin', 'lanacion'),
               categorias = c('economia', 'politica'),
               palabras_en_titulo = c('Crisis'),
               palabras_en_texto = c('conflicto','reactivación','corrup'))
```
### 4.2.
> "recuperar la **frecuencia** de la `persona` `'Cristina'` en `texto` , entre `2020-09-10` y `2020-09-12`, de los darios `Infobae` y `Página 12`, de las categorías `Internacional` y `Deporte`"
```
> f = frecuencias(que = 'personas',
                       donde = 'textos',
                       desde = '20200910',
                       hasta = '20200912',
                       diarios = c('clarin', 'lanacion'),
                       categorias = c('economia', 'politica'),
                       palabras = c('Cristina'))
```
### 3.3.
> "recuperar las **tendencia** de los primeros `20` `términos` en los `títulos` de la categoría `sociedad`, del diario `Clarin`, de la fecha `2021-01-16`"
```
> t = tendencias(que = 'terminos',
                 donde = 'titulos',
                 fecha = '20210116',
                 diarios = c('clarin'),
                 categorias = c('sociedad'),
                 top = 20)
```

- ***que***: "qué tipo de palabras buscar?" este paramétros puede ser '*personas*', '*terminos*' o '*verbos*'.
- ***donde***: "dónde buscar las palabras?" este paramétros puede ser '*titulos*' o '*textos*'.
- ***palabras***: "cuáles palabras buscar?" acá van las palabras que se quieren buscar. Apellido de una *persona*, o *término*, o verbo en *infinitivo*.
- ***desde***: "desde qué fecha buscar?" fecha desde la cual se quiere recuperar las frecuencias, en formato 'YYYYMMDD'.
- ***hasta***: "hasta qué fecha buscar?" fecha hastas la cual se quiere recuperar las frecuencias, en formato 'YYYYMMDD'.
- 
