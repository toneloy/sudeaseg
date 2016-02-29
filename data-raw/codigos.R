codigos_primas_cobradas <- c('50101', '52101', '53101', '54101', '54102')
codigos_primas_devueltas <- c('30103', '32102', '33102', '34102')
codigos_primas <- c(codigos_primas_cobradas ,codigos_primas_devueltas)

codigos_primas_personas <- c('50101', '30103')
codigos_primas_generales <- c('52101', '32102')
codigos_primas_solidarios <- c('53101', '33102')
codigos_primas_reaseguros <- c('54101', '54102', '34102')

codigos_siniestros_pagados <- c('30101', '30102', '32101', '33101', '34101')

codigos_siniestros_pendientes_personas <- c("4010401", "4010402", "4010403", "4010404")
codigos_siniestros_pendientes_solidarios <- c("4010406")
codigos_siniestros_pendientes_generales <- c("4010405")
codigos_siniestros_pendientes_reaseguros <- c("40105")

codigos_siniestros_pendientes <- c(
  codigos_siniestros_pendientes_personas,
  codigos_siniestros_pendientes_solidarios,
  codigos_siniestros_pendientes_generales,
  codigos_siniestros_pendientes_reaseguros
)

codigos_comisiones <- c('30104', '32103', '33103', '34103')
codigos_gastos_admin_personas <- '30109'
codigos_gastos_admin_generales <- '32109'
codigos_gastos_admin_solidarios <- '33109'
codigos_gastos_admin_reaseguros <- '34109'
codigos_gastos_admin_gestion <- '38109'

codigos_gastos_admin <- c(
  codigos_gastos_admin_personas,
  codigos_gastos_admin_solidarios,
  codigos_gastos_admin_generales,
  codigos_gastos_admin_reaseguros,
  codigos_gastos_admin_gestion
)

use_data(
  codigos_primas_cobradas,
  codigos_primas_devueltas,
  codigos_primas,
  codigos_primas_personas,
  codigos_primas_solidarios,
  codigos_primas_generales,
  codigos_primas_reaseguros,
  codigos_siniestros_pagados,
  codigos_siniestros_pendientes,
  codigos_comisiones,
  codigos_gastos_admin,
  internal = TRUE,
  overwrite = TRUE
)
