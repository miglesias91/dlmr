#' Conecta a la base y devuelve handler de la colección 
#'
#' @param coleccion nombre coleccion
#' @param db nombre db
#' @param usuario nombre usuario
#' @param password password usuario
#' @param servidor ip servidor
#' @param puerto puerto conexion
#' @param admindb admin donde esta guarda la info de loge
#' @import mongolite
#' @export
conexion = function(coleccion, db, usuario, password, servidor = 'localhost', puerto = 27017, admindb = 'admin') {
  
  if (servidor == 'localhost' | servidor == '127.0.0.1') {
    c = mongo(collection = coleccion, db = db)
    return(c)
  }
  
  url_path = paste0('mongodb://',usuario,':',password,'@',servidor,':',puerto,'/?authSource=',admindb)
  
  c = mongo(collection = coleccion, db = db, url = url_path)
  
  return(c)
}

#' Busca 'personas', 'terminos' o 'verbos' en 'textos' o 'titulos' de la coleccion, filtrando segun algunos parametros.
#'
#' @param conexion conexion a la coleccion 'frecuencias' ya instanciada
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
frecuencias = function(conexion, que, donde, palabras, desde = '0', hasta = '99999999', diarios = c(), categorias = c(), freq_min = 0, freq_max = 99999) {
  
  if (class(conexion)[1] != 'mongo') {
    stop('"conexion" no es una conexion de mongo.')
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
  
  resultado = conexion$aggregate(query)
  
  if (nrow(resultado) == 0) return(0)
  
  dresultado = as.data.table(resultado)
  
  columnas = c('fecha','diario','categoria','total', paste0(prefijo,'.', palabras))
  
  dresultado = dresultado[, ..columnas]
  
  setnames(dresultado, c('total', paste0(prefijo,'.',palabras)), c('noticias',palabras))
  
  return(dresultado)
}


#' Busca 'personas', 'terminos' o 'verbos' en 'textos' o 'titulos' de la coleccion, filtrando segun algunos parametros.
#'
#' @param conexion conexion a la coleccion 'frecuencias' ya instanciada
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
tendencias = function(conexion, que, donde, fecha, diarios = c(), categorias = c(), top = 10) {
  
  if (class(conexion)[1] != 'mongo') {
    stop('"conexion" no es una conexion de mongo.')
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
  
  resultado = conexion$aggregate(query)
  
  if (nrow(resultado) == 0) return(0)
  
  dresultado = data.table(palabra = names(resultado[1,2]), freq = as.numeric(resultado[1,2]))
  
  return(dresultado)
}