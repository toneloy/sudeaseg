serie <- function(data) {

  data %>%
    group_by(ano, mes) %>%
    summarise(monto = abs(sum(monto_neto))) %>%
    mutate(fecha = as.Date(sprintf('%s-%s-1', ano, mes))) %>%
    arrange(fecha) %>%
    ungroup()

}
