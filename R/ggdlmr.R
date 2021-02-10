#' Dibuja los datos de una tabla tipo 'clave:valor' en un gráfico estilo 'lollipop'.
#' El dataframe tiene que tener una columna 'clave' y otra 'valor'.
#'
#' @param d dataframe con tabla proveniente de la funcion 'tendencias'
#' @param color color del gráfico
#' @param titulo titulo del gráfico
#' @import ggplot2
#' @export
gglollipop = function(d, tamanio_texto = 15, colores = NULL, rango_y = NULL, separacion_lineas = 0.5, tamanio_puntos = 2, nombre_ejex = element_blank(), nombre_ejey = element_blank(), titulo = element_blank()) {
  
  if ('grupo2' %in% names(d)) {
    gg = ggplot(d, aes(x=reorder(clave, valor), y=valor), colour = grupo2) +
      geom_linerange(aes(x=reorder(clave, valor), ymin=0, ymax=valor, colour = grupo2),
                     position = position_dodge(width = separacion_lineas)) +
      geom_point(aes(colour = grupo2),
                 position = position_dodge(width = separacion_lineas), size=tamanio_puntos, alpha=1)   
  } else {
    gg = ggplot(d, aes(x=reorder(clave, valor), y=valor)) +
      geom_linerange(aes(x=reorder(clave, valor), ymin=0, ymax=valor),
                     position = position_dodge(width = separacion_lineas)) +
      geom_point(position = position_dodge(width = separacion_lineas), size=tamanio_puntos, alpha=1)
  }
  
  gg = gg +
    theme_light() +
    coord_flip(ylim = rango_y) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y = element_blank(),
      legend.title = element_blank(),
      text = element_text(size = tamanio_texto)
    ) +
    labs(title = titulo, x = nombre_ejey, y = nombre_ejex)
  
  if ('grupo' %in% names(d)) {
    gg = gg +facet_wrap(~grupo)
  }
  
  if (is.null(colores) == F) {
    gg + scale_color_manual(values = colores)
  }  
  else {
    gg
  }
}