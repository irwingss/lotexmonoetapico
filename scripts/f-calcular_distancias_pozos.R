# ==============================================================================
# FUNCIÓN PARA CALCULAR DISTANCIAS Y ORIENTACIONES A POZOS DE REFERENCIA
# ==============================================================================
# Creado: 2025-07-19
# Descripción: Calcula distancias en metros y orientaciones cardinales desde 
#              puntos de muestreo hasta pozos de referencia por locación
# ==============================================================================

#' Calcular distancia euclidiana entre dos puntos en coordenadas UTM
#' @param x1,y1 Coordenadas del punto 1 (ESTE, NORTE)
#' @param x2,y2 Coordenadas del punto 2 (ESTE, NORTE)
#' @return Distancia en metros
calcular_distancia_utm <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

#' Calcular orientación cardinal desde punto origen hacia punto destino
#' @param x1,y1 Coordenadas del punto origen (ESTE, NORTE)
#' @param x2,y2 Coordenadas del punto destino (ESTE, NORTE)
#' @return Orientación cardinal como texto
calcular_orientacion_cardinal <- function(x1, y1, x2, y2) {
  # Calcular diferencias
  dx <- x2 - x1
  dy <- y2 - y1
  
  # Calcular ángulo en radianes (0 = Este, π/2 = Norte)
  angulo_rad <- atan2(dy, dx)
  
  # Convertir a grados (0 = Este, 90 = Norte)
  angulo_grados <- angulo_rad * 180 / pi
  
  # Normalizar a 0-360 grados (0 = Norte, 90 = Este)
  angulo_normalizado <- (90 - angulo_grados) %% 360
  
  # Determinar orientación cardinal
  if (angulo_normalizado >= 337.5 || angulo_normalizado < 22.5) {
    return("norte")
  } else if (angulo_normalizado >= 22.5 && angulo_normalizado < 67.5) {
    return("noreste")
  } else if (angulo_normalizado >= 67.5 && angulo_normalizado < 112.5) {
    return("este")
  } else if (angulo_normalizado >= 112.5 && angulo_normalizado < 157.5) {
    return("sureste")
  } else if (angulo_normalizado >= 157.5 && angulo_normalizado < 202.5) {
    return("sur")
  } else if (angulo_normalizado >= 202.5 && angulo_normalizado < 247.5) {
    return("suroeste")
  } else if (angulo_normalizado >= 247.5 && angulo_normalizado < 292.5) {
    return("oeste")
  } else if (angulo_normalizado >= 292.5 && angulo_normalizado < 337.5) {
    return("noroeste")
  }
}

#' Generar texto descriptivo de distancia y orientación
#' @param distancia Distancia en metros (se redondea a entero)
#' @param orientacion Orientación cardinal
#' @param locacion Código de la locación
#' @return Texto descriptivo formateado
generar_texto_distancia <- function(distancia, orientacion, locacion) {
  distancia_redondeada <- round(distancia, 0)
  paste0("Punto de muestreo de suelo ubicado aproximadamente a ", 
         distancia_redondeada, " metros con dirección al ", 
         orientacion, " de la locación ", locacion)
}

#' Función principal para añadir distancias y altitudes a la muestra final
#' @param muestra_final DataFrame con la muestra final (debe tener LOCACION, ESTE, NORTE)
#' @param pozos_referencia DataFrame con pozos de referencia (LOCACION, ESTE, NORTE, ALTITUD)
#' @return DataFrame con columnas DISTANCIA y ALTITUD añadidas
añadir_distancias_pozos <- function(muestra_final, pozos_referencia) {
  
  # Validar que existan las columnas requeridas
  cols_requeridas_muestra <- c("LOCACION", "ESTE", "NORTE")
  cols_requeridas_pozos <- c("LOCACION", "ESTE", "NORTE", "ALTITUD")
  
  if (!all(cols_requeridas_muestra %in% names(muestra_final))) {
    stop("La muestra final debe contener las columnas: ", paste(cols_requeridas_muestra, collapse = ", "))
  }
  
  if (!all(cols_requeridas_pozos %in% names(pozos_referencia))) {
    stop("Los pozos de referencia deben contener las columnas: ", paste(cols_requeridas_pozos, collapse = ", "))
  }
  
  # Crear una copia de la muestra final para modificar
  resultado <- muestra_final
  
  # Inicializar las nuevas columnas
  resultado$DISTANCIA <- NA_character_
  resultado$ALTITUD <- NA_real_
  
  # Procesar cada locación
  locaciones_unicas <- unique(muestra_final$LOCACION)
  
  for (locacion in locaciones_unicas) {
    # Filtrar datos de la locación actual
    puntos_locacion <- muestra_final[muestra_final$LOCACION == locacion, ]
    pozo_referencia <- pozos_referencia[pozos_referencia$LOCACION == locacion, ]
    
    # Verificar que existe el pozo de referencia para esta locación
    if (nrow(pozo_referencia) == 0) {
      warning(paste("No se encontró pozo de referencia para la locación:", locacion))
      next
    }
    
    if (nrow(pozo_referencia) > 1) {
      warning(paste("Se encontraron múltiples pozos para la locación:", locacion, ". Se usará el primero."))
      pozo_referencia <- pozo_referencia[1, ]
    }
    
    # Obtener coordenadas del pozo de referencia
    x_pozo <- pozo_referencia$ESTE
    y_pozo <- pozo_referencia$NORTE
    altitud_pozo <- pozo_referencia$ALTITUD
    
    # Calcular distancias y orientaciones para todos los puntos de esta locación
    for (i in seq_len(nrow(puntos_locacion))) {
      x_punto <- puntos_locacion$ESTE[i]
      y_punto <- puntos_locacion$NORTE[i]
      
      # Calcular distancia
      distancia <- calcular_distancia_utm(x_pozo, y_pozo, x_punto, y_punto)
      
      # Calcular orientación (desde pozo hacia punto)
      orientacion <- calcular_orientacion_cardinal(x_pozo, y_pozo, x_punto, y_punto)
      
      # Generar texto descriptivo
      texto_distancia <- generar_texto_distancia(distancia, orientacion, locacion)
      
      # Asignar valores al resultado
      idx_resultado <- which(resultado$LOCACION == locacion & 
                            resultado$ESTE == x_punto & 
                            resultado$NORTE == y_punto)
      
      if (length(idx_resultado) > 0) {
        resultado$DISTANCIA[idx_resultado[1]] <- texto_distancia
        resultado$ALTITUD[idx_resultado[1]] <- altitud_pozo
      }
    }
  }
  
  return(resultado)
}
