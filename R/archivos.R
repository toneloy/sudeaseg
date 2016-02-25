corregir_formatos <- function(ifm) {

  ifm %>%
    mutate(mes = as.integer(mes)) %>%
    mutate(ano = as.integer(ano)) %>%
    mutate(monto = as.numeric(corregir_cero(monto))) %>%
    mutate(monto = convertir_bsf(monto, ano)) %>%
    mutate(depreciacion = as.numeric(corregir_cero(depreciacion))) %>%
    mutate(depreciacion = convertir_bsf(depreciacion, ano)) %>%
    mutate(monto_total = as.numeric(corregir_cero(monto_total))) %>%
    mutate(monto_total = convertir_bsf(monto_total, ano))
}

cargar_archivo <- function(file) {
  primera_linea = read.table(file, nrows = 1, header = FALSE, sep = ';', dec = '.', colClasses = 'character')
  if(ncol(primera_linea) == 1) {
    sep = ','
  } else {
    sep = ';'
  }

  data <- read.table(file, header = FALSE, sep = sep, colClasses = 'character') %>%
    tbl_df()

  data
}

cargar_directorio <- function(dir) {
  archivos <- list.files(dir, full.names = TRUE)

  ifm <- data_frame(
    codigo_empresa = character(),
    mes = integer(),
    ano = integer(),
    codigo_cuenta = character(),
    monto = numeric(),
    depreciacion = numeric(),
    monto_total = numeric()
  )

  n_archivos <- length(archivos)
  cargados <- 0

  for(archivo in archivos) {
    tabla <- cargar_archivo(archivo) %>%
      distinct() %>%
      rename(
        codigo_empresa = V1,
        mes = V2,
        ano = V3,
        codigo_cuenta = V4,
        monto = V5,
        depreciacion = V6,
        monto_total = V7
      ) %>% corregir_formatos()

    ifm %<>%
      bind_rows(tabla)
    cargados <- cargados + 1
    cat(sprintf('\r%s/%s archivos cargados\r', cargados, n_archivos))
  }

  ifm %>% distinct()
}

corregir_cero <- function(x) {
  gsub('0\\.00\\.', '0.00', x)
}

convertir_bsf <- function(x, ano) {
  ifelse(ano <= 2007, x / 1000, x)
}

consolidar_ifm <- function(ifm, codigo_contable, empresas) {
  ifm %>%
    inner_join(clasificar_cuentas(codigo_contable), by = 'codigo_cuenta') %>%
    inner_join(empresas, by = 'codigo_empresa') %>%
    mutate(monto_neto = monto_total * multiplicador)
}
