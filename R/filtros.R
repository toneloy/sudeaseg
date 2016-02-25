primas <- function(ifm) {
  ifm %>% filter(tipo_monto == 'Primas')
}

siniestros_pagados <- function(ifm) {
  ifm %>% filter(tipo_monto == 'Siniestros Pagados')
}

comisiones <- function(ifm) {
  ifm %>% filter(tipo_monto == 'Comisiones')
}
