ranking_areas <- function(data) {

  ranking_global <- ranking_global(data)

  data %>%
    mutate(empresa = factor(empresa, levels = ranking_global$empresa)) %>%
    group_by(empresa, tipo_monto, area_seguros) %>%
    summarise(monto = abs(sum(monto_neto))) %>%
    ungroup() %>%
    group_by(area_seguros) %>%
    mutate(ranking = min_rank(-monto))
}

ranking_global <- function(data) {

  data %<>%
    group_by(empresa, tipo_monto) %>%
    summarise(monto = abs(sum(monto_neto))) %>%
    ungroup() %>%
    arrange(desc(monto)) %>%
    mutate(ranking = min_rank(-monto)) %>%
    ungroup()

  data %>%
    mutate(empresa = factor(empresa, levels = data$empresa))

}

graficar_ranking_global <- function(data, ...) {
  tipo_monto <- data$tipo_monto[1]

  data %>%
    ranking_global() %>%
    ggvis(x = ~empresa, y = ~monto, ...) %>%
    layer_bars() %>%
    add_axis('x', title = 'Empresa', title_offset = 130, properties = axis_props(
      labels = list(angle = -90, align = 'right', dy = -6)
    )) %>%
    add_axis('y', title = tipo_monto, title_offset = 100) %>%
    add_tooltip(function(x) {
      sprintf(
        '%s<br/><b>Bs. %s</b>',
        x$x_, format(x$stack_upr_ - x$stack_lwr_, big.mark = '.', decimal.mark = ',', scientific = FALSE)
      )
    })
}

graficar_ranking_areas <- function(data, ...) {
  tipo_monto <- data$tipo_monto[1]

  data %>%
    ranking_areas() %>%
    ggvis(x = ~empresa, y = ~monto, fill = ~area_seguros, ...) %>%
    layer_bars() %>%
    add_axis('x', title = 'Empresa', title_offset = 130, properties = axis_props(
      labels = list(angle = -90, align = 'right', dy = -6)
    )) %>%
    add_axis('y', title = tipo_monto, title_offset = 100) %>%
    add_legend('fill', title = 'Área') %>%
    add_tooltip(function(x) {
      sprintf(
        '%s<br/>%s<br/><b>Bs. %s</b>',
        x$x_, x$area_seguros, format(x$stack_upr_ - x$stack_lwr_, big.mark = '.', decimal.mark = ',', scientific = FALSE)
      )
    })
}
