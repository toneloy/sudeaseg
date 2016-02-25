codigos_primas_cobradas <- c('50101', '52101', '53101', '54101', '54102')
codigos_primas_devueltas <- c('30103', '32102', '33102', '34102')
codigos_primas <- c(codigos_primas_cobradas ,codigos_primas_devueltas)

codigos_primas_personas <- c('50101', '30103')
codigos_primas_solidarios <- c('52101', '32102')
codigos_primas_generales <- c('53101', '33102')
codigos_primas_reaseguros <- c('54101', '54102', '34102')

codigos_siniestros_pagados <- c('30101', '30102', '32101', '33101', '34101')

codigos_comisiones <- c('30104', '32103', '33103', '34103')

use_data(
  codigos_primas_cobradas,
  codigos_primas_devueltas,
  codigos_primas,
  codigos_primas_personas,
  codigos_primas_solidarios,
  codigos_primas_generales,
  codigos_primas_reaseguros,
  codigos_siniestros_pagados,
  codigos_comisiones,
  internal = TRUE,
  overwrite = TRUE
)
