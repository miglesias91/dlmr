#' Conecta a la base y mantiene abierta la sesión
#'
#' @param usuario nombre usuario
#' @param password password usuario
#' @param servidor ip servidor
#' @param puerto puerto conexion
#' @param admindb admin donde esta guarda la info de loge
#' @import mongolite
#' @export
conectar = function(usuario, password, servidor = 'localhost', puerto = 27017, admindb = 'admin') {
  
  if(exists('conexion_dlmr')) {
    stop('Ya existe una conexión abierta!')
  }
  
  if (servidor == 'localhost' | servidor == '127.0.0.1') {
    
    tryCatch(
      expr = {
        cnoticias = mongo(collection = 'noticias', db = 'dlm')
        cfrecuencias = mongo(collection = 'frecuencias', db = 'resultados')
        
        conexion = list(noticias = cnoticias, frecuencias = cfrecuencias)
        assign('conexion_dlmr', conexion, .BaseNamespaceEnv)
      },
      error = function(e) {
        stop(paste0('No se pudo conectar al servidor: ',e))
      }
    )
    
    return('Conexión OK!')
  }
  
  url_path = paste0('mongodb://',usuario,':',password,'@',servidor,':',puerto,'/?authSource=',admindb)
  
  tryCatch(
    expr = {
      cnoticias = mongo(collection = 'noticias', db = 'dlm', url = url_path)
      cfrecuencias = mongo(collection = 'frecuencias', db = 'resultados', url = url_path)
      
      conexion = list(noticias = cnoticias, frecuencias = cfrecuencias)
      assign('conexion_dlmr', conexion, .BaseNamespaceEnv)
    },
    error = function(e) {
      stop(paste0('No se pudo conectar al servidor: ',e))
    }
  )
  
  return('Conexión OK!')
}

#' Busca 'personas', 'terminos' o 'verbos' en 'textos' o 'titulos' de la coleccion, filtrando segun algunos parametros.
#'
#' @param que qué buscar? 'personas', 'terminos' o 'verbos'
#' @param donde dónde buscar? 'textos' o 'titulos'
#' @param palabras palabras a buscar
#' @param desde fecha desde
#' @param hasta fecha hasta
#' @param diarios diarios a buscar
#' @param categorias categorias a buscar
#' @param freq_min freq minima de palabras a buscar
#' @param freq_max freq maxima de palabras a buscar
#' @import mongolite
#' @import stringi
#' @import data.table
#' @export
frecuencias = function(que, donde, palabras, desde = '0', hasta = '99999999', diarios = c(), categorias = c(), freq_min = 0, freq_max = 99999) {
  
  if(exists('conexion_dlmr') == F) {
    stop(paste0('NO existe una conexión abierta! Para inicar conexión llamar a "conectar()"'))
  }
  
  if (!(que %in% c('personas', 'terminos', 'verbos'))) {
    stop('"que" tiene que ser "personas", "terminos" o "verbos".')
  }
  
  if (!(donde %in% c('textos', 'titulos'))) {
    stop('"donde" tiene que ser "textos" o "titulos".')
  }
  
  if (is.null(palabras))  {
    stop('"palabras" tiene que contener al menos una palabra a filtrar.')
  }
  
  prefijo = ''
  if (que == 'personas' && donde == 'textos') prefijo = 'per_txt'
  if (que == 'personas' && donde == 'titulos') prefijo = 'per_tit'
  if (que == 'terminos' && donde == 'textos') prefijo = 'ter_txt'
  if (que == 'terminos' && donde == 'titulos') prefijo = 'ter_tit'
  if (que == 'verbos' && donde == 'textos') prefijo = 'ver_txt'
  if (que == 'verbos' && donde == 'titulos') prefijo = 'ver_tit'
  
  query_diario = paste0('"diario" : {"$in":[',stri_join(paste0('"',diarios,'"'), collapse = ','),']}')
  query_categoria = paste0('"categoria" : {"$in":[',stri_join(paste0('"',categorias,'"'), collapse = ','),']}')
  query_fecha = paste0('"fecha" : {"$gte" : "', desde,'", "$lte" : "', hasta,'"}')
  
  rango_freqs = paste0('{"$gte" : ', freq_min,', "$lte" : ', freq_max,'}')
  p = paste0('"',prefijo,'.', palabras, '":')
  ps = paste0(p, rango_freqs)
  query_palabras = stri_join(ps, collapse = ',')
  
  query_match = paste0('"$match" : {', query_palabras, ',', query_fecha)
  
  if (is.null(diarios) == F) query_match = paste0(query_match, ',', query_diario)
  if (is.null(categorias) == F) query_match = paste0(query_match, ',', query_categoria)
  
  query_match = paste0(query_match, '}')
  
  query = paste0('[{', query_match,'}]')
  
  resultado = conexion_dlmr$frecuencias$aggregate(query)
  
  if (nrow(resultado) == 0) return(0)
  
  dresultado = as.data.table(resultado)
  
  if(Sys.info()[1] == 'Windows') {
    dresultado = as.data.table(lapply(dresultado, iconv, from='utf-8', to='latin1'))
  }
  
  columnas = c('fecha','diario','categoria','total', paste0(prefijo,'.', palabras))
  
  dresultado = dresultado[, ..columnas]
  
  setnames(dresultado, c('total', paste0(prefijo,'.',palabras)), c('noticias',palabras))
  
  return(dresultado)
}

#' Busca 'personas', 'terminos' o 'verbos' en 'textos' o 'titulos' de la coleccion, filtrando segun algunos parametros.
#'
#' @param que qué buscar? 'personas', 'terminos' o 'verbos'
#' @param donde dónde buscar? 'textos' o 'titulos'
#' @param fecha fecha a medir tendencia
#' @param diarios diarios a buscar
#' @param categorias categorias a buscar
#' @param top traer las N palabras mas mencionadas
#' @import mongolite
#' @import stringi
#' @import data.table
#' @export
tendencias = function(que, donde, fecha, diarios = c(), categorias = c(), top = 10) {
  
  if(exists('conexion_dlmr') == F) {
    stop(paste0('NO existe una conexión abierta! Para inicar conexión llamar a "conectar()"'))
  }
  
  if (!(que %in% c('personas', 'terminos', 'verbos'))) {
    stop('"que" tiene que ser "personas", "terminos" o "verbos".')
  }
  
  if (!(donde %in% c('textos', 'titulos'))) {
    stop('"donde" tiene que ser "textos" o "titulos".')
  }
  
  prefijo = ''
  if (que == 'personas' && donde == 'textos') prefijo = 'per_txt'
  if (que == 'personas' && donde == 'titulos') prefijo = 'per_tit'
  if (que == 'terminos' && donde == 'textos') prefijo = 'ter_txt'
  if (que == 'terminos' && donde == 'titulos') prefijo = 'ter_tit'
  if (que == 'verbos' && donde == 'textos') prefijo = 'ver_txt'
  if (que == 'verbos' && donde == 'titulos') prefijo = 'ver_tit'
  
  query_diario = paste0('"diario" : {"$in":[',stri_join(paste0('"',diarios,'"'), collapse = ','),']}')
  query_categoria = paste0('"categoria" : {"$in":[',stri_join(paste0('"',categorias,'"'), collapse = ','),']}')
  query_fecha = paste0('"fecha" : "', fecha,'"')
  
  query_match = paste0('"$match" : {', query_fecha)
  
  if (is.null(diarios) == F) query_match = paste0(query_match, ',', query_diario)
  if (is.null(categorias) == F) query_match = paste0(query_match, ',', query_categoria)
  
  query_match = paste0(query_match, '}')
  
  ids = c('$fecha')
  if (is.null(diarios) == F) ids = c(ids, '$diario')
  if (is.null(categorias) == F) ids = c(ids, '$categoria')
  
  query_id = stri_join('[', stri_join(paste0('"',ids,'"'), collapse = ','),']')
  
  query = paste0('[',
  '{', query_match,'},
  { "$addFields": {
    "lista_aux": {"$objectToArray": "$', prefijo,'"} 
  } },
  { "$unwind": "$lista_aux"},
  { "$group": {
    "_id": {
      "id": ', query_id,',
      "k": "$lista_aux.k"
    },
    "v": {"$sum": "$lista_aux.v"}
  } },
  {"$sort" : {"v" : -1}},
  {"$limit": ', top,' },
  { "$group": {
    "_id": "$_id.id",
    "aux": { "$push": {"k": "$_id.k", "v": "$v"}}
  } },
  { "$project": {
    "tendencias": {"$arrayToObject": "$aux"}
  } }
  ]')
  
  resultado = conexion_dlmr$frecuencias$aggregate(query)
  
  if (nrow(resultado) == 0) return(0)
  
  dresultado = data.table(palabra = names(resultado[1,2]), freq = as.numeric(resultado[1,2]))
  
  if(Sys.info()[1] == 'Windows') {
    dresultado = as.data.table(lapply(dresultado, iconv, from='utf-8', to='latin1'))
  }
  
  return(dresultado)
}


#' Recupera noticias filtrando segun algunos parametros.
#'
#' @param desde fecha desde
#' @param hasta fecha hasta
#' @param diarios diarios a buscar
#' @param categorias categorias a buscar
#' @param palabras_en_titulo palabras que deben aparecen en el titulo (pueden ser pedazos de palabras)
#' @param palabras_en_texto palabras que deben aparecen en el texto (pueden ser pedazos de palabras)
#' @import mongolite
#' @import stringi
#' @import data.table
#' @export
noticias = function(desde, hasta, diarios = c(), categorias = c(), palabras_en_titulo = c(), palabras_en_texto = c()) {
  
  if(exists('conexion_dlmr') == F) {
    stop(paste0('NO existe una conexión abierta! Para inicar conexión llamar a "conectar()"'))
  }
  
  if (desde > hasta) {
    stop('La fecha "desde" debe ser menor o igual a la fecha "hasta".')
  }
  
  desde = paste0(substr(desde,1,4),'-',substr(desde,5,6),'-',substr(desde,7,8),'T00:00:00.000Z')
  hasta = paste0(substr(hasta,1,4),'-',substr(hasta,5,6),'-',substr(hasta,7,8),'T23:59:59.000Z')
  
  query_diario = paste0('"diario" : {"$in":[',stri_join(paste0('"',diarios,'"'), collapse = ','),']}')
  query_categoria = paste0('"cat" : {"$in":[',stri_join(paste0('"',categorias,'"'), collapse = ','),']}')
  query_fecha = paste0('"fecha" : {"$gte" : {"$date":"', desde,'"}, "$lte" : {"$date":"', hasta,'"}}')
  
  query_titulo = stri_join(paste0('"titulo":{"$regex":".*',palabras_en_titulo,'.*", "$options" : "i"}'), collapse = ',')
  query_texto = stri_join(paste0('"texto":{"$regex":".*',palabras_en_texto,'.*", "$options" : "i"}'), collapse = ',')
  
  query_find = paste0('{', query_fecha)
  
  if (is.null(diarios) == F) query_find = paste0(query_find, ',', query_diario)
  if (is.null(categorias) == F) query_find = paste0(query_find, ',', query_categoria)
  if (is.null(palabras_en_titulo) == F) query_find = paste0(query_find, ',', query_titulo)
  if (is.null(palabras_en_texto) == F) query_find = paste0(query_find, ',', query_texto)
  
  query = paste0(query_find, '}')
  
  noticias = conexion_dlmr$noticias$find(query)
  
  dnoticias = as.data.table(noticias)
  
  setnames(dnoticias, 'cat', 'categoria')
  
  if(Sys.info()[1] == 'Windows') {
    dnoticias = as.data.table(lapply(dnoticias, iconv, from='utf-8', to='latin1'))
  }
  
  return(dnoticias)
}