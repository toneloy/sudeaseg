#' Construye una serie de tiempo
#'
#' @param data Un data.frame.
#' @return Un \code{data.frame} con las columnas \code{tipo_monto}, \code{ano}, \code{mes}, \code{fecha} y \code{monto}
#' @examples
#' ifm %>%
#'   filter(mes == 12) %>%
#'   consolidar_ifm(codigo_contable, empresas) %>%
#'   filter(tipo_monto == 'Primas') %>%
#'   serie()
#' @description Construye una serie de tiempo en un \code{data.frame}. La \code{fecha} que devuelve es el primer día del mes indicado con \code{ano} y \code{mes} para que al graficar se visualice correctamente.

serie <- function(data) {

  data %>%
    group_by(tipo_monto, ano, mes) %>%
    summarise(monto = abs(sum(monto_neto))) %>%
    mutate(fecha = as.Date(sprintf('%s-%s-1', ano, mes))) %>%
    arrange(fecha) %>%
    ungroup() %>%
    select(tipo_monto, ano, mes, fecha, monto)

}

#' Construye series de tiempo por área de seguros
#'
#' @param data Un data.frame.
#' @return Un \code{data.frame} con las columnas \code{tipo_monto}, \code{area_seguros}, \code{ano}, \code{mes}, \code{fecha} y \code{monto}.
#' @examples
#' ifm %>%
#'   filter(mes == 12) %>%
#'   consolidar_ifm(codigo_contable, empresas) %>%
#'   filter(tipo_monto == 'Primas') %>%
#'   serie()
#' @description Construye series de tiempo por área de seguros en un \code{data.frame}. La \code{fecha} que devuelve es el primer día del mes indicado con \code{ano} y \code{mes} para que al graficar se visualice correctamente.

serie_areas <- function(data) {

  data %>%
    group_by(tipo_monto, area_seguros, ano, mes) %>%
    summarise(monto = abs(sum(monto_neto))) %>%
    mutate(fecha = as.Date(sprintf('%s-%s-1', ano, mes))) %>%
    arrange(fecha) %>%
    ungroup() %>%
    select(tipo_monto, area_seguros, ano, mes, fecha, monto)

}

#' Graficar serie de tiempo
#'
#' @param data Un data.frame.
#' @param data Un data.frame.
#' @return Un gráfico de líneas \code{ggvis} con \code{x = ~fecha} y \code{y = ~monto} y \code{fill = ~area_seguros}.
#' @examples
#' ifm %>%
#'   filter(mes == 12) %>%
#'   consolidar_ifm(codigo_contable, empresas) %>%
#'   filter(tipo_monto == 'Primas') %>%
#'   graficar_serie_areas()
#' @description Grafica series de tiempo por área de seguros utilizando \code{ggvis}.

graficar_serie <- function(data, titulo_y = 'Monto', tooltip = TRUE) {

  vis <- data %>%
    serie() %>%
    ggvis(x = ~fecha, y = ~monto) %>%
    layer_lines() %>%
    layer_points() %>%
    add_axis('x', title = '') %>%
    add_axis('y', title = titulo_y, title_offset = 100)

  if(tooltip) {
    vis %>%
      add_tooltip(function(x) {
        sprintf('%s-%s<br/><b>Bs. %s</b>', month(numerico_fecha(x$fecha)), year(numerico_fecha(x$fecha)), format(x$monto, big.mark = '.', decimal.mark = ',', scientific = FALSE))
      })
  } else {
    vis
  }

}

#' Graficar series de tiempo por área de seguros
#'
#' @param data Un data.frame.
#' @param titulo_y Un título para el eje de las y. Por defecto es 'Monto'
#' @param tooltip \code{logical}. Indica si se desea agregar un tooltip interactivo. Por defecto es \code{TRUE}.
#' @return Un gráfico de líneas \code{ggvis} con \code{x = ~fecha} y \code{y = ~monto} y \code{fill = ~area_seguros}.
#' @examples
#' ifm %>%
#'   filter(mes == 12) %>%
#'   consolidar_ifm(codigo_contable, empresas) %>%
#'   filter(tipo_monto == 'Primas') %>%
#'   graficar_serie_areas()
#' @description Grafica series de tiempo por área de seguros utilizando \code{ggvis}.

graficar_serie_areas <- function(data, titulo_y = 'Monto', tooltip = TRUE) {

  vis <- data %>%
    serie_areas() %>%
    ggvis(x = ~fecha, y = ~monto, stroke = ~area_seguros) %>%
    layer_lines() %>%
    layer_points(fill = ~area_seguros) %>%
    add_axis('x', title = '') %>%
    add_axis('y', title = titulo_y, title_offset = 100) %>%
    add_legend(c('fill', 'stroke'), title = 'Área')

  if(tooltip) {
    vis %>%
      add_tooltip(function(x) {
        sprintf('%s %s-%s<br/><b>Bs. %s</b>', x$area_seguros, month(numerico_fecha(x$fecha)), year(numerico_fecha(x$fecha)), format(x$monto, big.mark = '.', decimal.mark = ',', scientific = FALSE))
      })
  } else {
    vis
  }
}

numerico_fecha <- function(x) {
  as.Date(x / (24 * 60 * 60 * 1000), origin = as.Date('1970-01-01'))
}
