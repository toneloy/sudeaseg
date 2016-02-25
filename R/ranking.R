ranking_areas <- function(data) {

  ranking_global <- ranking_global(data)

  data %>%
    mutate(empresa = factor(empresa, levels = ranking_global$empresa)) %>%
    group_by(empresa, area_seguros) %>%
    summarise(monto = abs(sum(monto_neto))) %>%
    ungroup() %>%
    group_by(area_seguros) %>%
    mutate(ranking = min_rank(-monto))
}

ranking_global <- function(data) {

  data %<>%
    group_by(empresa) %>%
    summarise(monto = abs(sum(monto_neto))) %>%
    arrange(desc(monto)) %>%
    mutate(ranking = min_rank(-monto)) %>%
    ungroup()

  data %>%
    mutate(empresa = factor(empresa, levels = data$empresa))

}
