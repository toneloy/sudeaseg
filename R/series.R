serie <- function(data) {

  data %>%
    group_by(tipo_monto, ano, mes) %>%
    summarise(monto = abs(sum(monto_neto))) %>%
    mutate(fecha = as.Date(sprintf('%s-%s-1', ano, mes))) %>%
    arrange(fecha) %>%
    ungroup()

}

graficar_serie <- function(data) {
  tipo_monto <- data$tipo_monto[1]

  data %>%
    serie() %>%
    ggvis(x = ~fecha, y = ~monto) %>%
    layer_lines() %>%
    layer_points() %>%
    add_axis('x', title = '') %>%
    add_axis('y', title = tipo_monto, title_offset = 100) %>%
    add_tooltip(function(x) {
      sprintf('<b>Bs. %s</b>', format(x$monto, big.mark = '.', decimal.mark = ',', scientific = FALSE))
    })
}
