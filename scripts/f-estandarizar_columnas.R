# Función para estandarizar nombres de columnas
# Convierte todos los nombres de columnas a mayúsculas para consistencia
# en toda la aplicación, independientemente de cómo vengan en los archivos Excel

estandarizar_columnas <- function(df) {
  # Convertir todos los nombres de columnas a mayúsculas
  names(df) <- toupper(names(df))
  
  # Verificar y corregir nombres específicos que son críticos
  # para las tablas de marcos muestrales de celdas y grillas
  
  # Mapeo de posibles variaciones a nombres estándar
  mapeo_columnas <- list(
    # Coordenadas Este
    "ESTE" = c("este", "Este", "ESTE", "x", "X", "coord_x", "COORD_X"),
    # Coordenadas Norte  
    "NORTE" = c("norte", "Norte", "NORTE", "y", "Y", "coord_y", "COORD_Y"),
    # Profundidad
    "PROF" = c("prof", "Prof", "PROF", "profundidad", "Profundidad", "PROFUNDIDAD", "depth", "DEPTH"),
    # Código de celda
    "COD_CELDA" = c("cod_celda", "Cod_Celda", "COD_CELDA", "codigo_celda", "CODIGO_CELDA", "celda", "CELDA"),
    # Locación
    "LOCACION" = c("locacion", "Locacion", "LOCACION", "ubicacion", "UBICACION", "location", "LOCATION"),
    # Área
    "AREA" = c("area", "Area", "AREA", "superficie", "SUPERFICIE")
  )
  
  # Aplicar el mapeo
  nombres_actuales <- names(df)
  for (nombre_estandar in names(mapeo_columnas)) {
    variaciones <- mapeo_columnas[[nombre_estandar]]
    # Buscar si alguna variación existe en los nombres actuales
    for (variacion in variaciones) {
      if (variacion %in% nombres_actuales) {
        # Cambiar el nombre a la versión estándar
        names(df)[names(df) == variacion] <- nombre_estandar
        break  # Solo cambiar la primera coincidencia
      }
    }
  }
  
  # Mensaje informativo sobre las columnas encontradas
  cat("Columnas estandarizadas:", paste(names(df), collapse = ", "), "\n")
  
  return(df)
}

# Función auxiliar para verificar columnas requeridas
verificar_columnas_requeridas <- function(df, columnas_requeridas, nombre_archivo = "archivo") {
  columnas_faltantes <- setdiff(columnas_requeridas, names(df))
  
  if (length(columnas_faltantes) > 0) {
    mensaje_error <- paste0(
      "Error en ", nombre_archivo, ": Faltan las siguientes columnas requeridas: ",
      paste(columnas_faltantes, collapse = ", "),
      ". Columnas disponibles: ",
      paste(names(df), collapse = ", ")
    )
    stop(mensaje_error)
  }
  
  return(TRUE)
}
