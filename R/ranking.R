#' Calcular el ranking por empresa
#'
#' @param data Un data.frame con los datos a ser rankeados.
#' @return Un data.frame con el nombre de la empresa (como factor y ordenado según ranking)
#' @examples
#' consolidar_ifm(ifm, codigo_contable, empresas) %>%
#'   filter(tipo_monto == 'Primas', ano == 2014, mes == 12) %>%
#'   ranking_global()
#' @description Un data.frame con el nombre de la \code{empresa} (como factor y con niveles ordenados según ranking), el \code{tipo_monto}, el \code{monto} y el \code{ranking}.

ranking_global <- function(data) {

  data %>%
    group_by(empresa, tipo_monto, ano, mes) %>%
    summarise(monto = abs(sum(monto_neto))) %>%
    group_by(ano, mes) %>%
    arrange(desc(monto)) %>%
    mutate(ranking = min_rank(-monto)) %>%
    ungroup() %>%
    mutate(empresa = factor(empresa, levels = unique(empresa)))

}

#' Calcular el ranking por empresa según área de seguros
#'
#' @param data Un data.frame con los datos a ser rankeados.
#' @return Un data.frame con el nombre de la \code{empresa} (como factor y con niveles ordenados según ranking global), el \code{tipo_monto}, el \code{area_seguros} el \code{monto} y el \code{ranking}.
#' @examples
#' consolidar_ifm(ifm, codigo_contable, empresas) %>%
#'   filter(tipo_monto == 'Primas', ano == 2014, mes == 12) %>%
#'   ranking_areas()
#' @description Calcular el ranking de una variable por empresas según área de seguros (Personas, Generales, Solidarios y Reaseguros). Los datos deben ser de un \code{tipo_monto} específico Ej.: Primas, Siniestros Pagados, etc.

ranking_areas <- function(data) {

  ranking_global_df <- ranking_global(data)

  result <- data %>%
    group_by(empresa, tipo_monto, area_seguros, ano, mes) %>%
    summarise(monto = abs(sum(monto_neto))) %>%
    group_by(ano, mes, area_seguros) %>%
    mutate(ranking = min_rank(-monto)) %>%
    ungroup()

  if(is.data.frame(ranking_global_df)) {
    result %>% mutate(empresa = factor(empresa, levels = ranking_global_df$empresa))
  } else {
    result
  }
}

#' Graficar el ranking por empresa
#'
#' @param data Un data.frame con los datos a ser rankeados.
#' @param titulo_y Un título para el eje de las y. Por defecto es 'Monto'.
#' @param tooltip \code{logical}. Indica si se desea agregar un tooltip interactivo.
#' @param ... Parámetros opcionales para \code{ggvis}
#' @return Un gráfico de barras de \code{ggvis}.
#' @examples
#' ifm %>% filter(ano == 2014, mes == 12) %>%
#'   consolidar_ifm(codigo_contable, empresas) %>%
#'   filter(tipo_monto == 'Primas') %>%
#'   graficar_ranking_global()
#' @description Gráfico de barras \code{ggvis} del ranking de una variable por empresas con \code{x = ~empresa} y \code{y = ~monto}. Los datos deben ser de un \code{tipo_monto} específico Ej.: Primas, Siniestros Pagados, etc. Se agrega automáticamente un tooltip que aparece cuando se coloca el cursor sobre una barra.

graficar_ranking_global <- function(data, titulo_y = 'Monto', tooltip = TRUE, ...) {

  vis <- data %>%
    ranking_global() %>%
    ggvis(x = ~empresa, y = ~monto, ...) %>%
    layer_bars(stroke := NA) %>%
    add_axis('x', title = 'Empresa', title_offset = 130, properties = axis_props(
      labels = list(angle = -90, align = 'right', dy = -6)
    )) %>%
    add_axis('y', title = titulo_y, title_offset = 100)

  if(tooltip) {
    vis %>%
      add_tooltip(function(x) {
        sprintf(
          '%s<br/><b>Bs. %s</b>',
          x$x_, format(x$stack_upr_ - x$stack_lwr_, big.mark = '.', decimal.mark = ',', scientific = FALSE)
        )
      })
  } else {
    vis
  }

}

#' Graficar el ranking por empresa según área de seguros
#'
#' @param data Un data.frame con los datos a ser rankeados.
#' @param titulo_y Un título para el eje de las y. Por defecto es 'Monto'.
#' @param tooltip \code{logical}. Indica si se desea agregar un tooltip interactivo. Por defecto es \code{TRUE}.
#' @param ... Parámetros opcionales para \code{ggvis}
#' @return Un gráfico de barras de \code{ggvis}.
#' @examples
#' ifm %>% filter(ano == 2014, mes == 12) %>%
#'   consolidar_ifm(codigo_contable, empresas) %>%
#'   filter(tipo_monto == 'Primas') %>%
#'   graficar_ranking_areas()
#' @description Gráfico de barras \code{ggvis} del ranking de una variable por empresas según área de seguros, con \code{x = ~empresa}, \code{y = ~monto} y \code{fill = ~area_seguros}. Los datos deben ser de un \code{tipo_monto} específico Ej.: Primas, Siniestros Pagados, etc. Se agrega por defecto un tooltip que aparece cuando se coloca el cursor sobre una barra.

graficar_ranking_areas <- function(data, titulo_y = 'Monto', tooltip = TRUE, ...) {

  vis <- data %>%
    ranking_areas() %>%
    ggvis(x = ~empresa, y = ~monto, fill = ~area_seguros, ...) %>%
    layer_bars(stroke := NA) %>%
    add_axis('x', title = 'Empresa', title_offset = 130, properties = axis_props(
      labels = list(angle = -90, align = 'right', dy = -6)
    )) %>%
    add_axis('y', title = titulo_y, title_offset = 100) %>%
    add_legend('fill', title = 'Área')

  if(tooltip) {
    vis %>%
      add_tooltip(function(x) {
        sprintf(
          '%s<br/>%s<br/><b>Bs. %s</b>',
          x$x_, x$area_seguros, format(x$stack_upr_ - x$stack_lwr_, big.mark = '.', decimal.mark = ',', scientific = FALSE)
        )
      })
  } else {
    vis
  }
}
