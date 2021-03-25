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

#' Devuelve una 'foto' que resumén el estado de datos en la base.
#' @param diarios diarios a filtrar. Si no se especifica, se toman todos.
#' @param categorias categorias a filtar. Si no se especifica, se toman todos.
#' @param desde fecha desde
#' @param hasta fecha hasta
#' @import mongolite
#' @import data.table
#' @export
foto = function(diarios = c(), categorias = c(), desde = '00000000', hasta = '99999999') {
  fechas = paste0('"fecha" : {"$gte" : "', desde,'", "$lte" : "', hasta,'"}')
  
  fechas_por_parametro = F
  if (desde != '00000000' && hasta != '99999999') {
    dias = as.integer(difftime(as.Date(hasta, format='%Y%m%d'), as.Date(desde, format='%Y%m%d'), units = c('days'))) + 1
    fechas_por_parametro = T
  }
  
  if (is.null(diarios)) {
    diarios = conexion_dlmr$frecuencias$distinct('diario', paste0('{',fechas,'}'))
  }
  
  categorias_de_parametro = T
  if (is.null(categorias)) {
    categorias_de_parametro = F
  }
  
  query_addfields = '"$addFields": { "id_aux" : "aux"}'
  query_group = '"$group": { "_id": { "id": "$id_aux" }, "total": {"$sum": "$total"} }'
  
  dfoto = data.table(diario = character(),categoria = character(), desde = character(), hasta = character(),total = integer(), por_dia = numeric())
  for (diario in diarios) {
    
    if (diario == 'casarosada') {
      # falta corregir en base las frecuencias de casarosada de las fechas menores a '20190820160724'
      
      query_match = paste0('"$match" : { "diario" : "',diario,'",',fechas,' }')
      query = paste0('[ {',query_match,'}, {',query_addfields,'}, {',query_group,'} ]')

      total = conexion_dlmr$frecuencias$aggregate(query)$total

      if (fechas_por_parametro == F) {
        query_find = paste0('{"diario":"',diario,'"}')
        desde = conexion_dlmr$frecuencias$find(query_find, sort = '{"fecha":1}', limit = 1, fields = '{"fecha":1}')$fecha
        # desde = '20030525' # esto esta hardcodeado mal porque la fecha más vieja esta mal en la base. suponemos la fecha 'desde' como el primer discurso de nestor.
        hasta = conexion_dlmr$frecuencias$find(query_find, sort = '{"fecha":-1}', limit = 1, fields = '{"fecha":1}')$fecha

        dias = as.integer(difftime(as.Date(hasta, format='%Y%m%d'), as.Date(desde, format='%Y%m%d'), units = c('days'))) + 1
      }

      dfoto = rbind(dfoto, list(diario, 'todo', desde, hasta, total, round(total/dias, 5)))

      next
    }
    
    if (!categorias_de_parametro) {
      categorias = conexion_dlmr$frecuencias$distinct('categoria', paste0('{"diario":"',diario,'",',fechas,'}'))
    }
    
    for (categoria in categorias) {
      
      
      if (fechas_por_parametro == F) {
        query_find = paste0('{"diario":"',diario,'", "categoria":"',categoria,'"}')
        desde = conexion_dlmr$frecuencias$find(query_find, sort = '{"fecha":1}', limit = 1, fields = '{"fecha":1}')$fecha
        hasta = conexion_dlmr$frecuencias$find(query_find, sort = '{"fecha":-1}', limit = 1, fields = '{"fecha":1}')$fecha
        
        dias = as.integer(difftime(as.Date(hasta, format='%Y%m%d'), as.Date(desde, format='%Y%m%d'), units = c('days'))) + 1
      }
      
      query_match = paste0('"$match" : { "diario" : "',diario,'", "categoria":"',categoria,'",',fechas,' }')
      query = paste0('[ {',query_match,'}, {',query_addfields,'}, {',query_group,'} ]')
      
      total = conexion_dlmr$frecuencias$aggregate(query)$total
      
      dfoto = rbind(dfoto, list(diario, categoria, desde, hasta, total, round(total/dias,5)))
    }
  }
  
  return(dfoto[order(-total)])
}


#' Devuelve una 'foto' de las palabras en un rango de tiempo.
#' @param que qué buscar? 'personas', 'terminos' y/o 'verbos'
#' @param donde dónde buscar? 'textos' o 'titulos'
#' @param diarios diarios a filtrar. Si no se especifica, se toman todos.
#' @param categorias categorias a filtar. Si no se especifica, se toman todos.
#' @param desde fecha desde
#' @param hasta fecha hasta
#' @param top rankear las primeras N palabras
#' @import mongolite
#' @import data.table
#' @export
foto_palabras = function(que, donde, diarios = c(), categorias = c(), desde = '00000000', hasta = '99999999', top = 2000, top_por_tendencia = 500,freq_min = 5, min_noticias = 1, por_categoria = T) {
  fechas = paste0('"fecha" : {"$gte" : "', desde,'", "$lte" : "', hasta,'"}')
  
  fechas_por_parametro = F
  if (desde != '00000000' && hasta != '99999999') {
    dias = as.integer(difftime(as.Date(hasta, format='%Y%m%d'), as.Date(desde, format='%Y%m%d'), units = c('days'))) + 1
    
    if (dias > 30) {
      stop('El rango de días no puede ser mayor a 30.')
    }
    
    fechas_por_parametro = T
  } else {
    desde = format(as.Date(Sys.time()) - 30, '%Y%m%d')
    hasta = format(Sys.time(), '%Y%m%d')
  }
  
  if (is.null(diarios)) {
    diarios = conexion_dlmr$frecuencias$distinct('diario', paste0('{',fechas,'}'))
  }
  
  categorias_de_parametro = T
  if (is.null(categorias)) {
    categorias_de_parametro = F
  }
  
  terminos_y_personas = data.table()
  for (diario in diarios) {
    if (diario == 'casarosada') {
      
      total_noticias = contar(desde = desde,
                              hasta = hasta,
                              diarios = c(diario))
      
      if (total_noticias < min_noticias) next
      
      t_y_p = tendencias(que = que,
                         donde = donde,
                         desde = desde,
                         hasta = hasta,
                         diarios = c(diario),
                         top = top_por_tendencia)
      
      if(por_categoria) {
        t_y_p[,c('diario', 'categoria','por_noticia') := list(diario, 'todo', round(freq/total_noticias,5))]
      } else {
        t_y_p[,c('diario', 'por_noticia') := list(diario, round(freq/total_noticias,5))]
      }
      
      terminos_y_personas = rbind(terminos_y_personas, t_y_p)
      
      next
    }
    
    if(por_categoria){
    
      if (!categorias_de_parametro) {
        categorias = conexion_dlmr$frecuencias$distinct('categoria', paste0('{"diario":"',diario,'",',fechas,'}'))
      }
      
      for (categoria in categorias) {
        
        total_noticias = contar(desde = desde,
                                hasta = hasta,
                                diarios = c(diario),
                                categorias = c(categoria))
        
        if (total_noticias < min_noticias) next
        
        t_y_p = tendencias(que = que,
                           donde = donde,
                           desde = desde,
                           hasta = hasta,
                           diarios = c(diario),
                           categorias = c(categoria),
                           top = top_por_tendencia)
        
        t_y_p[,c('diario', 'categoria','por_noticia') := list(diario, categoria, round(freq/total_noticias,5))]
        terminos_y_personas = rbind(terminos_y_personas, t_y_p)
      }
    } else {
      total_noticias = contar(desde = desde,
                              hasta = hasta,
                              diarios = c(diario))
      
      if (total_noticias < min_noticias) next
      
      t_y_p = tendencias(que = que,
                         donde = donde,
                         desde = desde,
                         hasta = hasta,
                         diarios = c(diario),
                         top = top_por_tendencia)
      
      t_y_p[,c('diario', 'por_noticia') := list(diario, round(freq/total_noticias,5))]
    }
    terminos_y_personas = rbind(terminos_y_personas, t_y_p)
  }
  
  if (nrow(terminos_y_personas[freq > freq_min]) > top) {
    return(terminos_y_personas[freq > freq_min][order(-por_noticia)][1:top])
  } else {
    return(terminos_y_personas[freq > freq_min][order(-por_noticia)])
  }
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
  
  if (desde > hasta) {
    stop('La fecha "desde" debe ser menor o igual a la fecha "hasta".')
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
    names(dresultado) = iconv(names(dresultado), from='utf-8', to='latin1')
  }
  
  palabras_sin_espacios = gsub(' ', '.', palabras)
  
  columnas = c('fecha','diario','categoria','total', paste0(prefijo,'.', palabras_sin_espacios))
  
  dresultado = dresultado[, ..columnas]
  
  setnames(dresultado, c('total', paste0(prefijo,'.',palabras_sin_espacios)), c('noticias',palabras))
  
  return(dresultado)
}

#' Busca 'personas', 'terminos' o 'verbos' en 'textos' o 'titulos' de la coleccion, filtrando segun algunos parametros.
#'
#' @param que qué buscar? 'personas', 'terminos' o 'verbos'. Puede ser 1, 2 o los 3.
#' @param donde dónde buscar? 'textos' o 'titulos'
#' @param fecha fecha a medir tendencia
#' @param diarios diarios a buscar
#' @param categorias categorias a buscar
#' @param top traer las N palabras mas mencionadas
#' @import mongolite
#' @import stringi
#' @import data.table
#' @export
tendencias = function(que, donde, desde, hasta, diarios = c(), categorias = c(), top = 10) {
  
  if(exists('conexion_dlmr') == F) {
    stop(paste0('NO existe una conexión abierta! Para inicar conexión llamar a "conectar()"'))
  }
  
  if (!all(que %in% c('personas', 'terminos', 'verbos'))) {
    stop('"que" tiene que contener "personas", "terminos" y/o "verbos".')
  }
  
  if (!(donde %in% c('textos', 'titulos'))) {
    stop('"donde" tiene que ser "textos" o "titulos".')
  }
  
  if (desde > hasta) {
    stop('La fecha "desde" debe ser menor o igual a la fecha "hasta".')
  }
  
  prefijos = c()
  if ('personas' %in% que && donde == 'textos') prefijos = c(prefijos, 'per_txt')
  if ('personas' %in% que && donde == 'titulos') prefijos = c(prefijos, 'per_tit')
  if ('terminos' %in% que && donde == 'textos') prefijos = c(prefijos, 'ter_txt')
  if ('terminos' %in% que && donde == 'titulos') prefijos = c(prefijos, 'ter_tit')
  if ('verbos' %in% que && donde == 'textos') prefijos = c(prefijos, 'ver_txt')
  if ('verbos' %in% que && donde == 'titulos') prefijos = c(prefijos, 'ver_tit')
  
  query_diario = paste0('"diario" : {"$in":[',stri_join(paste0('"',diarios,'"'), collapse = ','),']}')
  query_categoria = paste0('"categoria" : {"$in":[',stri_join(paste0('"',categorias,'"'), collapse = ','),']}')
  
  if (desde != hasta)
    query_fecha = paste0('"fecha" : {"$gte" : "', desde,'", "$lte" : "', hasta,'"}')
  else
    query_fecha = paste0('"fecha" : "', desde,'"')
  
  query_match = paste0('"$match" : {', query_fecha)
  
  if (is.null(diarios) == F) query_match = paste0(query_match, ',', query_diario)
  if (is.null(categorias) == F) query_match = paste0(query_match, ',', query_categoria)
  
  query_match = paste0(query_match, '}')
  
  query_addfields = paste0('"$addFields": { "lista_aux": {"$concatArrays": [',stri_join(paste0('{"$objectToArray": "$',prefijos,'"}'), collapse = ','), ']}')
  # query_addfields = paste0('"$addFields": { "lista_aux": {"$objectToArray": "$', prefijo,'"}')
  if (desde != hasta)
    query_addfields = paste0(query_addfields, ', "id_aux" : "aux"')
  
  query_addfields = paste0(query_addfields, '}')
  
  if (desde != hasta)
    ids = c('$id_aux')
  else
    ids = c('$fecha')
  
  if (is.null(diarios) == F) ids = c(ids, '$diario')
  if (is.null(categorias) == F) ids = c(ids, '$categoria')
  
  query_id = stri_join('[', stri_join(paste0('"',ids,'"'), collapse = ','),']')
  
  query = paste0('[
  {',query_match,'},
  {',query_addfields,'},
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

#' Cuenta noticias filtrando segun algunos parametros.
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
contar = function(desde, hasta, diarios = c(), categorias = c(), palabras_en_titulo = c(), palabras_en_texto = c()) {
  
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
  
  total = conexion_dlmr$noticias$count(query)
  
  return(total)
}

#' Formatea la tabla según donde se quiera presentar
#'
#' @param tabla tabla a formatear
#' @param para tipo de formato segun donde se quiera publicar
#' @param cantidad_decimales cantidad de decimales a tomar. -1 para dejar la misma cantidad y que quede como numeric
#' @import stringi
#' @import data.table
#' @export
formatear = function(data, para = 'blog', cantidad_decimales = 2) {
  if(para == 'blog') {
    columnas = names(data)
    for(columna in columnas) {
      if (class(data[[columna]]) == 'numeric' && cantidad_decimales > 0) {
        data[[columna]] = sprintf(paste0('%.',cantidad_decimales,'f'),data[[columna]])
      }
      if (columna == 'diario') {
        data[get(columna) == 'lanacion', (columna) := 'La Nación']
        data[get(columna) == 'infobae', (columna) := 'Infobae']
        data[get(columna) == 'clarin', (columna) := 'Clarín']
        data[get(columna) == 'diariodeleuco', (columna) := 'Le Doy Mi Palabra']
        data[get(columna) == 'eldestape', (columna) := 'El Destape']
        data[get(columna) == 'todonoticias', (columna) := 'TN']
        data[get(columna) == 'popular', (columna) := 'Popular']
        data[get(columna) == 'paginadoce', (columna) := 'Página 12']
        data[get(columna) == 'telam', (columna) := 'Télam']
        data[get(columna) == 'perfil', (columna) := 'Perfil']
        data[get(columna) == 'ambito', (columna) := 'Ámbito']
        data[get(columna) == 'casarosada', (columna) := 'Casa Rosada']
      }
    }
  }
  
  return(data)
}