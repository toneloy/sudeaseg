calcular_area_negocio <- function(codigo_cuenta) {
  codigo_cuenta <- as.character(codigo_cuenta)
  resultado <- rep(NA, length(codigo_cuenta))
  resultado <- ifelse(
    substr(codigo_cuenta, 1, 2) %in% apply(expand.grid(c(3, 5), c(0, 2, 3, 4)), 1, function(x) paste(x[1], x[2], sep = "")),
    'Seguros',
    'Gestión general'
  )
  resultado
}

calcular_area_seguros <- function(codigo_cuenta, area_negocio) {
  codigo_cuenta <- as.character(codigo_cuenta)
  resultado <- rep(NA, length(codigo_cuenta))
  resultado <- ifelse(area_negocio == 'Seguros' & substr(codigo_cuenta, 2, 2) == '0', 'Personas', resultado)
  resultado <- ifelse(area_negocio == 'Seguros' & substr(codigo_cuenta, 2, 2) == '2', 'Generales', resultado)
  resultado <- ifelse(area_negocio == 'Seguros' & substr(codigo_cuenta, 2, 2) == '3', 'Solidarios', resultado)
  resultado <- ifelse(area_negocio == 'Seguros' & substr(codigo_cuenta, 2, 2) == '4', 'Reaseguros', resultado)
  # resultado <- factor(resultado, levels = c('Generales', 'Personas', 'Reaseguros', 'Solidarios'))
  resultado
}

calcular_tipo_cuenta <- function(codigo_cuenta) {
  resultado <- rep(NA, length(codigo_cuenta))
  resultado <- ifelse(substr(codigo_cuenta, 1, 1) == '2', 'Activos', resultado)
  resultado <- ifelse(substr(codigo_cuenta, 1, 1) == '3', 'Egresos', resultado)
  resultado <- ifelse(substr(codigo_cuenta, 1, 1) == '4', 'Pasivos', resultado)
  resultado <- ifelse(substr(codigo_cuenta, 1, 1) == '5', 'Ingresos', resultado)
  return(resultado)
}

calcular_multiplicador <- function(codigo_cuenta) {
  codigo_cuenta <- as.character(codigo_cuenta)
  resultado <- rep(NA, length(codigo_cuenta))
  resultado <- ifelse(substr(codigo_cuenta, 1, 1) %in% c('2', '5'), 1, -1)
  resultado
}

calcular_tipo_monto <- function(codigo_cuenta) {
  codigo_cuenta <- as.character(codigo_cuenta)
  resultado <- rep(NA, length(codigo_cuenta))
  resultado <- ifelse(codigo_cuenta %in% codigos_primas, 'Primas', resultado)
  resultado <- ifelse(codigo_cuenta %in% codigos_siniestros_pagados, 'Siniestros Pagados', resultado)
  resultado <- ifelse(codigo_cuenta %in% codigos_comisiones, 'Comisiones', resultado)
  resultado
}

calcular_nivel <- function(codigo_cuenta) {
  resultado <- (nchar(codigo_cuenta) - 1) / 2

}

clasificar_cuentas <- function(data) {

  data %>%
    mutate(multiplicador = calcular_multiplicador(codigo_cuenta)) %>%
    mutate(area_negocio = calcular_area_negocio(codigo_cuenta)) %>%
    mutate(area_seguros = calcular_area_seguros(codigo_cuenta, area_negocio)) %>%
    mutate(tipo_monto = calcular_tipo_monto(codigo_cuenta)) %>%
    mutate(tipo_cuenta = calcular_tipo_cuenta(codigo_cuenta)) %>%
    mutate(nivel = calcular_nivel(codigo_cuenta))
}
