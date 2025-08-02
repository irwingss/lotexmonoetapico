# --- Cargar Paquetes --- #
# Cargar todos los paquetes requeridos para la aplicación
# ---------------------------------------------------------------------------- #
packages <- c(
  "shiny", "shinydashboard", "readxl", "DT", "dplyr", 
  "TeachingSampling", "dbscan", "purrr", "openxlsx", "sf",
  "colourpicker", "uuid"
)

# Cargar todos los paquetes requeridos
lapply(packages, library, character.only = TRUE)
# ---------------------------------------------------------------------------- #

# Cargar funciones y scripts
# ---------------------------------------------------------------------------- #
# NOTA: ESTE bloque carga todas las funciones y parámetros necesarios
# para la ejecución de la app. Se ejecuta una sola vez al inicio.
# ---------------------------------------------------------------------------- #
tryCatch({
  scripts_path <- "scripts"
  params_path <- "Parámetros generales"
  
  # Cargar scripts de funciones (estos no requieren lme4)
  r_scripts <- list.files(path = scripts_path, pattern = "\\.R$", full.names = TRUE, ignore.case = TRUE)
  # Excluir scripts que son para análisis interactivo y no para la app
  scripts_a_excluir <- c("Revisión de listado de locaciones.R")
  r_scripts <- r_scripts[!grepl(paste(scripts_a_excluir, collapse="|"), r_scripts)]
  for (script in r_scripts) {
    print(paste("Cargando script:", script))
    source(script, local = TRUE, encoding = "UTF-8")
  }
  
  # Cargar directamente el archivo RData con los parámetros precalculados
  rdata_path <- file.path(params_path, "parametros.RData")
  print(paste("Cargando parámetros desde:", rdata_path))
  load(rdata_path, envir = environment())
  
  print("Todos los scripts y parámetros han sido cargados exitosamente.")
  
}, error = function(e) {
  # Manejo de errores en caso de que un script falle
  stop(paste("Error al cargar los scripts iniciales:", e$message))
})

# Definir la Interfaz de Usuario (UI)
ui <- navbarPage(
  title = tagList("Diseño Monoetápico"),
  header = tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles_v2.css"),
      # Agregar fuente Roboto desde Google Fonts
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;500;700&display=swap"),
      # Script para aplicar animaciones
      tags$script("
        $(document).on('shiny:connected', function() {
          $('.fade-in').addClass('fade-in');
        });
      ")
    ),
    # Indicador de simulación activa
    uiOutput("indicador_simulacion")
  ),
  
  # Pestaña 1 - Carga de datos y Análisis
  tabPanel("1. Carga inicial y Percentil", 
           fluidRow(
             column(width = 3, # Columna lateral (30%)
                    wellPanel(class = "fade-in",
                      h3("1A. Cargar Archivo Excel", class = "fade-in"),
                      div(class = "card",
                        fileInput("archivo_excel", "Carga el archivo Excel de celdas preliminares",
                                  accept = c(".xlsx", ".xls"),
                                  buttonLabel = "Examinar...",
                                  placeholder = "Ningún archivo seleccionado"),
                        p(strong("Columnas:"), "LOCACION, AREA, COD_CELDA"),
                        actionButton("cargar_btn", "Cargar datos", 
                                    class = "btn-primary btn-block")
                      ),
                      tags$hr(),
                      div(class = "card",
                        textInput("locacion_simular", "Simular eliminación de locación", value = ""),
                        fluidRow(
                          column(width = 6,
                            actionButton("simular_btn", "Simular", class = "btn-warning btn-block")
                          ),
                          column(width = 6,
                            actionButton("revertir_btn", "Revertir", icon = icon("undo"), class = "btn-info btn-block")
                          )
                        )
                      ),
                      tags$hr(),
                      h3("1B. Análisis de Percentiles", class = "fade-in"),
                      div(class = "card",
                        textInput("area_rejilla_input", "Valores de área de rejilla a evaluar:",
                                 value = "3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 20, 25, 30"),
                        actionButton("calcular_btn", "Calcular Percentiles", 
                                    class = "btn-primary btn-block")
                      ),
                      tags$hr(),
                      uiOutput("opciones_analisis")
                    )
             ),
             column(width = 9, # Área principal (70%)
                    tabsetPanel(id = "tabset_principal",
                      tabPanel("Tabla de Celdas Cargadas", 
                               h3("Vista previa de datos", class = "fade-in"),
                               div(class = "card fade-in",
                                 DTOutput("preview_datos")
                               )),
                      tabPanel("Tabla de Percentiles", 
                               h3("Tabla de Percentiles", class = "fade-in"),
                               div(class = "card fade-in",
                                 DTOutput("tabla_percentiles")
                               )),
                       tabPanel("Revisión de Celdas", 
                               h3("Revisión de Celdas según Umbral", class = "fade-in"),
                               fluidRow(
                                 # Columna izquierda para datos y resumen
                                 column(width = 5,
                                        # Resultados seleccionados estilizados
                                        div(class = "card fade-in",
                                          uiOutput("resultados_estilizados")
                                        ),
                                        
                                        # Resumen de la revisión
                                        div(class = "card fade-in",
                                          uiOutput("resumen_revision")
                                        )
                                 ),
                                 
                                 # Columna derecha para la tabla
                                 column(width = 7,
                                        # Tabla de conteo de celdas por locación
                                        div(class = "card fade-in",
                                          h4("Conteo de Celdas por Locación"),
                                          DTOutput("tabla_conteo_locaciones")
                                        )
                                 )
                               )
                      )
                    )
             )
           )
  ),
  
  # Pestaña 2 - Carga y verificación de marcos
  tabPanel("2. Carga de Marcos",
           fluidRow(
             column(width = 3, # Columna lateral (30%)
                    wellPanel(class = "fade-in",
                      h3("2A. Cargar Archivos", class = "fade-in"),
                      div(class = "card",
                        p(strong("Columnas requeridas:")),
                        p(em("Marco de Celdas:"), " LOCACION, COD_CELDA, PROF"),
                        p(em("Marco de Grillas:"), " LOCACION, COD_CELDA, COD_GRILLA, P_SUPERPOS, ESTE, NORTE, PROF"),
                        p(strong("Nota:"), " Los nombres de columnas se estandarizan automáticamente a mayúsculas al cargar los archivos.")
                      ),
                      tags$hr(),
                      div(class = "card",
                        fileInput("archivo_marco_celdas", "Seleccionar archivo Excel de marco de CELDAS",
                                  accept = c(".xlsx", ".xls")),
                        fileInput("archivo_marco_grillas", "Seleccionar archivo Excel de marco de GRILLAS",
                                  accept = c(".xlsx", ".xls")),
                        actionButton("cargar_marcos_btn", "Cargar marcos", 
                                    class = "btn-primary btn-block")
                      ),
                      tags$hr(),
                      h3("2B. Verificación de Marcos", class = "fade-in"),
                      div(class = "card",
                        actionButton("verificar_marcos_btn", "Verificar integridad", 
                                    class = "btn-success btn-block")
                      )
                    )
             ),
             column(width = 9, # Área principal (70%)
                    tabsetPanel(id = "tabset_fase2",
                      tabPanel("Vista Previa", 
                               fluidRow(
                                 column(width = 6,
                                        h3("Marco de Celdas", class = "fade-in"),
                                        div(class = "card fade-in",
                                          DTOutput("preview_marco_celdas")
                                        )),
                                 column(width = 6,
                                        h3("Marco de Grillas", class = "fade-in"),
                                        div(class = "card fade-in",
                                          DTOutput("preview_marco_grillas")
                                        ))
                               )),
                      tabPanel("Verificación de Locaciones", 
                               h3("Verificación de Locaciones", class = "fade-in"),
                               fluidRow(
                                 column(width = 6,
                                        div(class = "card fade-in",
                                          h4("Conteo de Celdas por Locación"),
                                          DTOutput("conteo_celdas_locacion"),
                                          downloadButton("download_conteo_celdas", "Descargar XLSX", class = "btn-sm btn-info")
                                        )),
                                 column(width = 6,
                                        div(class = "card fade-in",
                                          h4("Locaciones sin Celdas"),
                                          verbatimTextOutput("locaciones_sin_celdas"),
                                          downloadButton("download_locaciones_sin_celdas", "Descargar XLSX", class = "btn-sm btn-info"),
                                          uiOutput("resumen_locaciones")
                                        ))
                               )),
                      tabPanel("Verificación de Grillas", 
                               h3("Verificación de Grillas", class = "fade-in"),
                               fluidRow(
                                 column(width = 6,
                                        div(class = "card fade-in",
                                          h4("Conteo de Grillas por Celda"),
                                          DTOutput("conteo_grillas_celda"),
                                          downloadButton("download_conteo_grillas", "Descargar XLSX", class = "btn-sm btn-info")
                                        )),
                                 column(width = 6,
                                        div(class = "card fade-in",
                                          h4("Celdas con menos de 3 Grillas"),
                                          verbatimTextOutput("celdas_pocas_grillas"),
                                          downloadButton("download_celdas_pocas_grillas", "Descargar XLSX", class = "btn-sm btn-info"),
                                          uiOutput("resumen_grillas")
                                        ))
                               )),
                      tabPanel("Verificación Cruzada", 
                               h3("Verificación Cruzada entre Marcos", class = "fade-in"),
                               fluidRow(
                                 column(width = 6,
                                        div(class = "card fade-in",
                                          h4("Celdas en marco_celdas pero no en marco_grillas"),
                                          DTOutput("celdas_no_en_grillas"),
                                          downloadButton("download_celdas_no_en_grillas", "Descargar XLSX", class = "btn-sm btn-info"),
                                          uiOutput("sugerencia_celdas_no_en_grillas")
                                        )),
                                 column(width = 6,
                                        div(class = "card fade-in",
                                          h4("Celdas en marco_grillas pero no en marco_celdas"),
                                          DTOutput("celdas_no_en_marco"),
                                          downloadButton("download_celdas_no_en_marco", "Descargar XLSX", class = "btn-sm btn-info"),
                                          uiOutput("sugerencia_celdas_no_en_marco"),
                                          uiOutput("resumen_verificacion_cruzada")
                                        ))
                               ))
                    )
             )
           )
  ),
  
  # Pestaña 3 - Cálculo del tamaño muestral
  tabPanel("3. Cálculo del n muestral",
           fluidRow(
             column(width = 3, # Columna lateral (30%)
                    wellPanel(class = "fade-in",
                      h3("3A. Parámetros de Muestreo", class = "fade-in"),
                      div(class = "card",
                        numericInput("nivel_confianza", "Nivel de confianza (%)", 95, min = 80, max = 99.9, step = 0.1),
                        numericInput("tasa_no_respuesta", "Tasa de no respuesta (%)", 5.75, min = 0, max = 50, step = 0.01),
                        numericInput("margen_error", "Margen de error (% de la media)", 15, min = 1, max = 50, step = 0.1),
                        tags$hr(),
                        actionButton("calcular_n_btn", "Calcular tamaño muestral", 
                                    class = "btn-primary btn-block")
                      )
                    )
             ),
             column(width = 9, # Área principal (70%)
                    tabsetPanel(id = "tabset_fase3",
                      tabPanel("Resultados", 
                               h3("Cálculo del tamaño muestral", class = "fade-in"),
                               fluidRow(
                                 column(width = 6,
                                        div(class = "card fade-in",
                                          h4("Resultados del cálculo"),
                                          uiOutput("resultado_n_muestral")
                                        )),
                                 column(width = 6,
                                        div(class = "card fade-in",
                                          h4("Fórmula utilizada"),
                                          withMathJax(uiOutput("formula_n_muestral"))
                                        ))
                               ),
                               div(class = "card fade-in",
                                 h4("Parámetros utilizados en el cálculo"),
                                 verbatimTextOutput("parametros_n_muestral")
                               ))
                    )
             )
           )
  ),
  
  # Pestaña 4 - Muestreo Monoetápico
  tabPanel("4. Muestreo Monoetápico",
           fluidRow(
             column(width = 3, # Columna lateral (30%)
                    wellPanel(class = "fade-in",
                      h3("4A. Ejecutar Muestreo", class = "fade-in"),
                      div(class = "card",
                        p("Selecciona las celdas y rejillas finales usando los marcos y el tamaño de muestra definidos."),
                        numericInput("seed_muestreo", "Semilla para reproducibilidad:", value = 123, min = 1),
                        actionButton("ejecutar_muestreo_btn", "1. Ejecutar Muestreo Bietápico", 
                                     class = "btn-success btn-block")
                      ),
                      tags$hr(),
                      h3("4B. Generar Códigos", class = "fade-in"),
                      div(class = "card",
                        p("Añade los códigos de campo y colectora a la muestra, optimizando el orden de supervisión."),
                        actionButton("generar_codigos_btn", "2. Generar Códigos de Campo", 
                                     class = "btn-primary btn-block")
                      ),
                      tags$hr(),
                      h3("4C. Generar distancias a Pozos", class = "fade-in"),
                      div(class = "card",
                        p("Carga un archivo Excel con pozos de referencia para calcular distancias y añadir altitudes."),
                        p(strong("Columnas requeridas:")),
                        p(em("LOCACION, ESTE, NORTE, ALTITUD")),
                        fileInput("archivo_pozos_referencia", "Seleccionar archivo Excel de pozos",
                                  accept = c(".xlsx", ".xls")),
                        actionButton("generar_distancias_btn", "3. Generar Distancias y Altitudes", 
                                     class = "btn-warning btn-block")
                      ),
                      tags$hr(),
                      h3("4D. Exportar Resultados", class = "fade-in"),
                      div(class = "card",
                        p("Descarga la muestra final con códigos en el formato que prefieras."),
                        downloadButton("descargar_shp_btn", "Descargar Shapefile (.zip)", class = "btn-success btn-block"),
                        tags$br(),
                        tags$br(),
                        downloadButton("descargar_muestra_btn", "Descargar Excel (.xlsx)", class = "btn-info btn-block")
                      )
                    )
             ),
             column(width = 9, # Área principal (70%)
                    tabsetPanel(id = "tabset_fase4",
                                tabPanel("Resumen del Muestreo", 
                                         h3("Verificación de la Muestra Final", class = "fade-in"),
                                         fluidRow(
                                           column(width = 6,
                                                  div(class = "card fade-in",
                                                      h4("Estadísticas Generales"),
                                                      DTOutput("tabla_estadisticas_generales")
                                                  )
                                           ),
                                           column(width = 6,
                                                  div(class = "card fade-in",
                                                      h4("Conteo de Rejillas por Locación"),
                                                      DTOutput("tabla_conteo_rejillas_locacion"),
                                                      tags$br(),
                                                      downloadButton("descargar_conteo_rejillas_btn", "Descargar Tabla (.xlsx)", 
                                                                    class = "btn-success btn-sm")
                                                  )
                                           )
                                         )
                                ),
                                tabPanel("Muestra Final", 
                                         h3("Rejillas Seleccionadas", class = "fade-in"),
                                         div(class = "card fade-in",
                                           DTOutput("tabla_muestra_final")
                                         )
                                )
                    )
             )
           )
  ),
  
  # Pestaña de Errores - Para mostrar errores de la aplicación
  tabPanel("⚠️ Errores",
           fluidRow(
             column(width = 12,
                    div(class = "card fade-in",
                        h3("Registro de Errores de la Aplicación", class = "fade-in"),
                        p("Esta pestaña muestra todos los errores que han ocurrido durante la ejecución de la aplicación."),
                        tags$hr(),
                        h4("Errores Recientes:"),
                        div(id = "error-container", style = "max-height: 600px; overflow-y: auto;",
                            verbatimTextOutput("registro_errores")
                        ),
                        tags$hr(),
                        fluidRow(
                          column(width = 6,
                                 actionButton("limpiar_errores_btn", "Limpiar Registro", 
                                             class = "btn-warning", icon = icon("trash"))
                          ),
                          column(width = 6,
                                 downloadButton("descargar_errores_btn", "Descargar Log de Errores", 
                                               class = "btn-info")
                          )
                        )
                    )
             )
           )
  )
)

# Definir la lógica del servidor
server <- function(input, output, session) {
  # Variables reactivas - Fase 1
  marco_celdas_original <- reactiveVal(NULL)
  marco_celdas_backup <- reactiveVal(NULL)  # Para guardar una copia antes de simular
  locacion_simulada <- reactiveVal("")  # Para guardar la locación que se está simulando eliminar
  bd_percentiles_completa <- reactiveVal(NULL)
  umbral_elegido <- reactiveVal(NULL)
  a_rejilla <- reactiveVal(NULL)
  lado_rejilla <- reactiveVal(NULL)
  marco_celdas_filtrado <- reactiveVal(NULL)
  conteo_locaciones <- reactiveVal(NULL)
  
  # Variables reactivas - Fase 2
  marco_celdas <- reactiveVal(NULL)
  marco_grillas <- reactiveVal(NULL)
  conteo_celdas_por_locacion <- reactiveVal(NULL)
  conteo_grillas_por_celda <- reactiveVal(NULL)
  locaciones_faltantes <- reactiveVal(NULL)
  celdas_con_pocas_grillas <- reactiveVal(NULL)
  celdas_solo_en_marco_celdas <- reactiveVal(NULL)
  celdas_solo_en_marco_grillas <- reactiveVal(NULL)
  
  # Variables reactivas - Fase 4 (Pozos de referencia)
  pozos_referencia <- reactiveVal(NULL)
  datos_finales_con_distancias <- reactiveVal(NULL)
  
  # Sistema de manejo de errores
  registro_errores_lista <- reactiveVal(list())
  
  # Función para registrar errores
  registrar_error <- function(error_obj, contexto = "") {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    
    # Extraer información detallada del error
    mensaje_error <- ""
    if (is.character(error_obj)) {
      mensaje_error <- error_obj
    } else if (inherits(error_obj, "error")) {
      mensaje_error <- conditionMessage(error_obj)
      if (nchar(mensaje_error) == 0) {
        mensaje_error <- paste("Error de tipo:", class(error_obj)[1])
      }
    } else {
      mensaje_error <- as.character(error_obj)
    }
    
    # Si el mensaje sigue vacío, proporcionar información básica
    if (nchar(mensaje_error) == 0 || mensaje_error == "") {
      mensaje_error <- "Error desconocido - sin mensaje específico"
    }
    
    # Añadir información adicional si está disponible
    if (inherits(error_obj, "error") && !is.null(error_obj$call)) {
      call_info <- deparse(error_obj$call)[1]
      if (nchar(call_info) > 0) {
        mensaje_error <- paste(mensaje_error, "\nLlamada:", call_info)
      }
    }
    
    nuevo_error <- list(
      timestamp = timestamp,
      contexto = contexto,
      mensaje = mensaje_error
    )
    
    errores_actuales <- registro_errores_lista()
    errores_actuales <- append(errores_actuales, list(nuevo_error))
    
    # Mantener solo los últimos 50 errores para evitar problemas de memoria
    if (length(errores_actuales) > 50) {
      errores_actuales <- errores_actuales[(length(errores_actuales) - 49):length(errores_actuales)]
    }
    
    registro_errores_lista(errores_actuales)
  }
  
  # Función para cargar los datos cuando se presione el botón
  observeEvent(input$cargar_btn, {
    req(input$archivo_excel)
    
    # Leer el archivo Excel
    tryCatch({
      datos <- read_excel(input$archivo_excel$datapath)
      
      # Estandarizar nombres de columnas (convertir a mayúsculas y mapear variaciones)
      datos <- estandarizar_columnas(datos)
      
      # Verificar que existan las columnas requeridas para celdas preliminares
      verificar_columnas_requeridas(datos, c("LOCACION", "AREA", "COD_CELDA"), "archivo de celdas preliminares")
      
      marco_celdas_original(datos) # Guardar en la variable reactiva
      showNotification("Archivo cargado exitosamente con columnas estandarizadas", type = "message")
      }, error = function(e) {
        registrar_error(e, "Carga de Archivo Excel")
        showNotification(paste("Error al cargar el archivo:", conditionMessage(e)), type = "error")
      })
  })
  
  # Función para simular la eliminación de una locación
  observeEvent(input$simular_btn, {
    req(marco_celdas_original(), input$locacion_simular)
{{ ... }}
    
    # Verificar que la locación exista
    locacion_a_eliminar <- input$locacion_simular
    datos_actuales <- marco_celdas_original()
    
    if (!locacion_a_eliminar %in% unique(datos_actuales$LOCACION)) {
      showNotification(paste("La locación", locacion_a_eliminar, "no existe en los datos"), type = "error")
      return()
    }
    
    # Guardar una copia de los datos originales antes de simular
    if (locacion_simulada() == "") { # Solo guardar backup si no hay una simulación activa
      marco_celdas_backup(datos_actuales)
    }
    
    # Filtrar los datos para eliminar la locación seleccionada
    datos_filtrados <- datos_actuales %>%
      filter(LOCACION != locacion_a_eliminar)
    
    # Actualizar los datos y guardar la locación simulada
    marco_celdas_original(datos_filtrados)
    locacion_simulada(locacion_a_eliminar)
    
    # Notificar al usuario
    showNotification(
      paste("Simulando sin la locación:", locacion_a_eliminar, 
            "(Eliminadas", nrow(datos_actuales) - nrow(datos_filtrados), "filas)"),
      type = "warning",
      duration = 5
    )
    
    # Limpiar los resultados de percentiles para que se recalculen
    bd_percentiles_completa(NULL)
    umbral_elegido(NULL)
    a_rejilla(NULL)
    lado_rejilla(NULL)
    marco_celdas_filtrado(NULL)
    conteo_locaciones(NULL)
  })
  
  # Función para revertir la simulación
  observeEvent(input$revertir_btn, {
    req(marco_celdas_backup())
    
    # Restaurar los datos originales
    marco_celdas_original(marco_celdas_backup())
    
    # Notificar al usuario
    if (locacion_simulada() != "") {
      showNotification(
        paste("Se ha restaurado la locación:", locacion_simulada()),
        type = "message",
        duration = 5
      )
    } else {
      showNotification("No hay cambios que revertir", type = "warning")
    }
    
    # Limpiar la variable de locación simulada
    locacion_simulada("")
    
    # Limpiar los resultados de percentiles para que se recalculen
    bd_percentiles_completa(NULL)
    umbral_elegido(NULL)
    a_rejilla(NULL)
    lado_rejilla(NULL)
    marco_celdas_filtrado(NULL)
    conteo_locaciones(NULL)
  })
  
  # Indicador de simulación activa
  output$indicador_simulacion <- renderUI({
    if (locacion_simulada() != "") {
      div(class = "card fade-in", style = "background-color: #FFC107; color: #000; padding: 10px; margin-bottom: 10px; text-align: center;",
          icon("exclamation-triangle"), strong("MODO SIMULACIÓN: "), 
          paste("Se ha eliminado temporalmente la locación", locacion_simulada()))
    } else {
      return(NULL)
    }
  })
  
  # Mostrar la vista previa de los datos
  output$preview_datos <- renderDT({
    req(marco_celdas_original())
    datatable(marco_celdas_original(), 
              options = list(pageLength = 12, 
                            scrollX = TRUE,
                            autoWidth = TRUE))
  })
  
  # Función para calcular la tabla de percentiles
  observeEvent(input$calcular_btn, {
    req(marco_celdas_original())
    
    tryCatch({
      # Verificar si existen las variables necesarias
      if (!exists("base_tph_umbral_fil")) {
        # Si no existe, crear datos simulados para demostración
        base_tph_umbral_fil <<- data.frame(
          TPH = rnorm(100, mean = 500, sd = 100),
          locacion = sample(LETTERS[1:10], 100, replace = TRUE)
        )
        showNotification("Usando datos simulados para TPH", type = "warning")
      }
      
      if (!exists("DEFF_extended")) {
        DEFF_extended <<- 1.5  # Valor simulado
        showNotification("Usando valor simulado para DEFF_extended", type = "warning")
      }
      
      # Obtener los valores de area_rejilla del input
      area_rejilla_valores <- as.numeric(unlist(strsplit(input$area_rejilla_input, "[,\\s]+")))
      
      # Crear la función empírica de distribución acumulada
      F_empirica <- ecdf(marco_celdas_original()$AREA)
      
      # Crear la tabla desde area_rejilla
      bd_percentiles <- data.frame(opcion = 1:length(area_rejilla_valores),
                                  area_rejilla = area_rejilla_valores) %>% 
        dplyr::mutate(area_celda = area_rejilla * 3,
               percentil = F_empirica(area_celda) * 100)
      
      # Función de calculo de n basado en rejillas y celdas
      calculo_n <- function(celda, rejilla) {
        Z <- 1.96 
        TNR <- 0.0575 
        med <- mean(base_tph_umbral_fil$TPH, na.rm = TRUE)
        e <- 0.15 * med 
        σ <- sd(base_tph_umbral_fil$TPH, na.rm = TRUE) 
        N <- marco_celdas_original() %>% 
          filter(AREA >= celda) %>% 
          mutate(rejillas_que_contiene = floor(AREA/rejilla)) %>% 
          pull(rejillas_que_contiene) %>% 
          sum()
        muestra <- round(((N * Z ^ 2 * σ ^ 2) / (e ^ 2 * (N - 1) + Z ^ 2 * σ ^ 2)) * (1 / (1 - TNR)) * DEFF_extended)
        return(muestra)
      }
      
      # Función de celdas que se van
      celdas_sevan <- function(marco, umbral_elegido) {
        conteo <- marco %>% 
          filter(AREA >= umbral_elegido) %>% 
          pull(LOCACION) %>% 
          unique() %>% 
          length()
        return(conteo)
      }
      
      # Calcular n_estimado y Locaciones_remanentes
      bd_percentiles <- bd_percentiles %>% 
        dplyr::rowwise() %>% 
        dplyr::mutate(n_estimado = tryCatch({
          calculo_n(area_celda, area_rejilla)
        }, error = function(e) {
          NA_integer_
        })) %>% 
        as.data.frame() %>% 
        dplyr::ungroup()
      
      bd_percentiles <- bd_percentiles %>% 
        dplyr::rowwise() %>% 
        dplyr::mutate(Locaciones_remanentes = tryCatch({
          celdas_sevan(marco_celdas_original(), area_celda)
        }, error = function(e) {
          NA_integer_
        })) %>% 
        dplyr::ungroup()
        
      # Redondear valores con 2 decimales, excepto columnas de números enteros
      bd_percentiles <- bd_percentiles %>% 
        dplyr::mutate(
          percentil = round(percentil, 2),
          area_celda = round(area_celda, 2),
          area_rejilla = round(area_rejilla, 2)
        ) %>% 
        # Reordenar las columnas en el orden solicitado
        dplyr::select(opcion, area_rejilla, area_celda, percentil, n_estimado, Locaciones_remanentes)
      
      # Guardar la tabla en la variable reactiva
      bd_percentiles_completa(bd_percentiles)
      
      # Mostrar notificación
      showNotification("Tabla de percentiles calculada exitosamente", type = "message")
      
      # Cambiar a la pestaña de Tabla de Percentiles
      updateTabsetPanel(session, "tabset_principal", selected = "Tabla de Percentiles")
      
    }, error = function(e) {
      showNotification(paste("Error al calcular percentiles:", e$message), type = "error")
    })
  })
  
  # Mostrar la tabla de percentiles
  output$tabla_percentiles <- renderDT({
    req(bd_percentiles_completa())
    datatable(bd_percentiles_completa(), 
              options = list(
                pageLength = 20, 
                scrollX = TRUE,
                autoWidth = FALSE, # Cambiar a FALSE para evitar ajuste automático
                fixedHeader = TRUE, # Fijar encabezados
                columnDefs = list(
                  list(targets = c(1, 2, 3), # Columnas percentil, area_celda, area_rejilla (0-indexado)
                       render = JS("function(data, type, row, meta) { return type === 'display' ? parseFloat(data).toFixed(2) : data; }")
                  ),
                  # Definir anchos fijos para cada columna
                  list(targets = 0, width = '60px'), # opcion
                  list(targets = 1, width = '100px'), # area_rejilla
                  list(targets = 2, width = '100px'), # area_celda
                  list(targets = 3, width = '100px'), # percentil
                  list(targets = 4, width = '100px'), # n_estimado
                  list(targets = 5, width = '150px')  # Locaciones_remanentes
                ),
                # Alinear correctamente los encabezados de columnas
                headerCallback = JS(
                  "function(thead, data, start, end, display) {",
                  "  $(thead).find('th').css('text-align', 'center');",
                  "  $(thead).find('th').css('vertical-align', 'middle');",
                  "}"
                ),
                # Comprimir la altura de las filas
                rowCallback = JS(
                  "function(row, data) {",
                  "  $(row).css('line-height', '80%');",
                  "  $(row).css('padding-top', '2px');",
                  "  $(row).css('padding-bottom', '2px');",
                  "  $(row).find('td').css('text-align', 'center');", # Centrar contenido de celdas
                  "}"
                )
              ),
              selection = 'single')
  })
  
  # Generar las opciones de selección
  output$opciones_analisis <- renderUI({
    req(bd_percentiles_completa())
    
    # Crear el título dinámico con el rango entre paréntesis
    titulo_input <- paste0("Seleccionar opción (entre 1 y ", nrow(bd_percentiles_completa()), "):")
    
    tagList(
      textInput("fila_seleccionada", titulo_input, 
                value = "10"), # Por defecto seleccionamos la fila 10
      actionButton("confirmar_seleccion", "Confirmar selección", 
                   class = "btn-success btn-block")
    )
  })
  
  # Actualizar valores cuando se confirma la selección
  observeEvent(input$confirmar_seleccion, {
    req(bd_percentiles_completa(), input$fila_seleccionada, marco_celdas_original())
    
    # Obtener la fila seleccionada y validar
    fila_texto <- input$fila_seleccionada
    
    # Verificar si es un número válido
    if(!grepl("^\\d+$", fila_texto)) {
      showNotification("Por favor, ingrese un número entero válido", type = "error")
      return()
    }
    
    # Convertir a número
    fila <- as.numeric(fila_texto)
    
    # Verificar que esté dentro del rango válido
    if(fila < 1 || fila > nrow(bd_percentiles_completa())) {
      showNotification(paste("Por favor, ingrese un número entre 1 y", nrow(bd_percentiles_completa())), type = "error")
      return()
    }
    
    # Extraer los valores como escalares
    area_celda_valor <- as.numeric(bd_percentiles_completa()[fila, "area_celda"])
    
    # Guardar el umbral elegido
    umbral_elegido(area_celda_valor)
    
    # Calcular a_rejilla
    a_rejilla_valor <- area_celda_valor / 3
    a_rejilla(a_rejilla_valor)
    
    # Calcular el lado de la rejilla
    lado_rejilla(sqrt(a_rejilla_valor))
    
    # Mostrar notificación
    showNotification("Selección confirmada. Valores actualizados.", type = "message")
    
    # Ejecutar automáticamente la revisión de celdas
    tryCatch({
      # Identificar celdas que se van (por debajo del umbral)
      celdas_que_se_van <- marco_celdas_original() %>% 
        filter(AREA < area_celda_valor) %>% 
        pull(COD_CELDA) %>% 
        unique()
      
      # Filtrar el marco de celdas
      pre_marco_celdas <- marco_celdas_original() %>%
        filter(!COD_CELDA %in% celdas_que_se_van)
      
      # Guardar el marco filtrado
      marco_celdas_filtrado(pre_marco_celdas)
      
      # Contar celdas por locación y calcular estadísticas de área
      conteo_por_locacion <- pre_marco_celdas %>% 
        group_by(LOCACION) %>%
        summarise(
          n = n(),
          Min_Area = round(min(AREA, na.rm = TRUE),2),
          Max_Area = round(max(AREA, na.rm = TRUE), 2),
          Promed_Area = round(mean(AREA, na.rm = TRUE), 2)
        ) %>%
        arrange(n)
      
      # Guardar el conteo
      conteo_locaciones(conteo_por_locacion)
      
      # Cambiar a la pestaña de Revisión de Celdas
      updateTabsetPanel(session, "tabset_principal", selected = "Revisión de Celdas")
      
    }, error = function(e) {
      showNotification(paste("Error al revisar celdas:", e$message), type = "error")
    })
  })
  
  # Mostrar los resultados seleccionados con estilo mejorado
  output$resultados_estilizados <- renderUI({
    req(umbral_elegido(), a_rejilla(), lado_rejilla())
    
    # Convertir a valores escalares
    umbral <- as.numeric(umbral_elegido())
    area <- as.numeric(a_rejilla())
    lado <- as.numeric(lado_rejilla())
    
    tagList(
      h4("Valores Seleccionados"),
      tags$div(
        tags$p(
          "", 
          tags$span(style = "color: green; font-weight: bold; font-size: 100%;", "Área de celda elegida (m2): "), 
          tags$strong(format(umbral, digits = 6))
        ),
        tags$p(
          "Área de rejilla (m2): ", 
          tags$strong(format(area, digits = 6))
        ),
        tags$p(
          "Lado (raíz cudrada del área de rejilla, m): ", 
          tags$strong(format(lado, digits = 6))
        )
      )
    )
  })
  
  # Mantener la versión anterior para compatibilidad
  output$resultados_seleccionados <- renderPrint({
    req(umbral_elegido(), a_rejilla(), lado_rejilla())
    
    # Convertir a valores escalares para evitar problemas con cat()
    umbral <- as.numeric(umbral_elegido())
    area <- as.numeric(a_rejilla())
    lado <- as.numeric(lado_rejilla())
    
    cat("Área de la celda:", format(umbral, digits = 6), "\n")
    cat("Área de rejilla:", format(area, digits = 6), "\n")
    cat("Lado de la rejilla:", format(lado, digits = 6), "\n")
  })
  
  # Mantener ESTE bloque vacío para referencia futura
  # La funcionalidad de revisión de celdas ahora está integrada en el evento confirmar_seleccion
  # observeEvent(input$revisar_celdas_btn, { ... })
  
  # Mostrar resumen de la revisión
  output$resumen_revision <- renderUI({
    req(marco_celdas_original(), marco_celdas_filtrado(), conteo_locaciones())
    
    # Calcular estadísticas
    total_locaciones_original <- length(unique(marco_celdas_original()$LOCACION))
    total_locaciones_filtrado <- length(unique(marco_celdas_filtrado()$LOCACION))
    total_celdas_original <- nrow(marco_celdas_original())
    total_celdas_filtrado <- nrow(marco_celdas_filtrado())    
    
    # Identificar locaciones sin celdas después del filtrado
    locaciones_originales <- unique(marco_celdas_original()$LOCACION)
    locaciones_filtradas <- unique(marco_celdas_filtrado()$LOCACION)
    locaciones_sin_celdas <- setdiff(locaciones_originales, locaciones_filtradas)
    
    # Verificar si hay locaciones con exactamente una celda
    locaciones_con_una_celda <- conteo_locaciones() %>% 
      filter(n == 1) %>% 
      nrow() > 0
    
    # Crear el resumen
    tagList(
      h4("Resumen de la Revisión"),
      p(paste("Total de locaciones en el marco original:", total_locaciones_original)),
      p(paste("Total de locaciones en el marco filtrado:", total_locaciones_filtrado)),
      p(paste("Total de celdas en el marco original:", total_celdas_original)),
      p(paste("Total de celdas en el marco filtrado:", total_celdas_filtrado)),
      p(paste("Celdas eliminadas:", total_celdas_original - total_celdas_filtrado)),
      
      if(length(locaciones_sin_celdas) > 0) {
        tagList(
          tags$div(style = "color: red; font-weight: bold;",
                   "ADVERTENCIA: Las siguientes locaciones se quedaron sin celdas:"),
          tags$ul(
            lapply(locaciones_sin_celdas, function(loc) {
              tags$li(loc)
            })
          )
        )
      } else {
        tagList(
          tags$div(style = "color: green; font-weight: bold;",
                 "Todas las locaciones tienen al menos una celda."),
          
          # Solo mostrar la sugerencia si hay locaciones con exactamente una celda
          if(locaciones_con_una_celda) {
            tags$div(style = "margin-top: 10px; padding: 10px; background-color: #ffcdda; border-left: 5px solid #ff0730; color: #800412;",
              tags$i(class = "fa fa-exclamation-triangle", style = "margin-right: 5px;"),
              "LOCACIÓN CON UNA CELDA: Solo le queda una (01) celda a una locación [tienen muchas celdas pequeñas]. Vuelve a SIG y considera las celdas pequeñas como rejillas. Copia el polígono de las celda pequeñas hacia el shapefile de rejillas. Dales  códigos de rejilla, un único código de celda, y exporta nuevamente los marcos.")
          }
        )
      }
    )
  })
  
  # Mostrar tabla de conteo de celdas por locación
  output$tabla_conteo_locaciones <- renderDT({
    req(conteo_locaciones())
    datatable(conteo_locaciones(), 
              options = list(
                pageLength = 12, 
                scrollX = TRUE,
                autoWidth = FALSE,  # Cambiar a FALSE para evitar el reajuste automático
                fixedHeader = TRUE, # Mantener los encabezados fijos
                columnDefs = list(  # Definir anchos fijos para las columnas
                  list(targets = 0, width = '150px'),  # LOCACION
                  list(targets = 1, width = '50px'),   # n
                  list(targets = 2, width = '80px'),   # Min_Area
                  list(targets = 3, width = '80px'),   # Max_Area
                  list(targets = 4, width = '80px')    # Promed_Area
                ),
                rowCallback = JS(
                  "function(row, data) {",
                  "  $(row).css('line-height', '80%');",
                  "  $(row).css('padding-top', '2px');",
                  "  $(row).css('padding-bottom', '2px');",
                  "}"
                )
              )
            )
  })
  
  # ============================================================================ #
  # FASE 2: CARGA Y VERIFICACIÓN DE MARCOS                                       #
  # ============================================================================ #
  
  # Función para cargar los marcos cuando se presione el botón
  observeEvent(input$cargar_marcos_btn, {
    # Verificar que ambos archivos estén cargados
    if (is.null(input$archivo_marco_celdas) || is.null(input$archivo_marco_grillas)) {
      showNotification("Debe seleccionar ambos archivos Excel", type = "error")
      return()
    }
    
    # Leer los archivos Excel
    tryCatch({
      # Cargar marco de celdas
      datos_celdas <- read_excel(input$archivo_marco_celdas$datapath)
      # Estandarizar nombres de columnas
      datos_celdas <- estandarizar_columnas(datos_celdas)
      # Verificar columnas requeridas para marco de celdas
      verificar_columnas_requeridas(datos_celdas, c("COD_CELDA", "LOCACION"), "marco de celdas")
      marco_celdas(datos_celdas)
      
      # Cargar marco de grillas
      datos_grillas <- read_excel(input$archivo_marco_grillas$datapath)
      # Estandarizar nombres de columnas
      datos_grillas <- estandarizar_columnas(datos_grillas)
      # Verificar columnas requeridas para marco de grillas (ESTE, NORTE, PROF son clave)
      verificar_columnas_requeridas(datos_grillas, c("COD_CELDA", "ESTE", "NORTE", "PROF"), "marco de grillas")
      marco_grillas(datos_grillas)
      
      # Mostrar notificación de éxito
      showNotification("Marcos cargados exitosamente con columnas estandarizadas", type = "message")
      
      # Cambiar a la pestaña de Vista Previa
      updateTabsetPanel(session, "tabset_fase2", selected = "Vista Previa")
      
    }, error = function(e) {
      registrar_error(e, "Carga de Marcos")
      showNotification(paste("Error al cargar los archivos:", conditionMessage(e)), type = "error")
    })
  })
  
  # Mostrar la vista previa de los marcos
  output$preview_marco_celdas <- renderDT({
    req(marco_celdas())
    datatable(marco_celdas(), 
              options = list(pageLength = 10, 
                            scrollX = TRUE,
                            autoWidth = TRUE))
  })
  
  output$preview_marco_grillas <- renderDT({
    req(marco_grillas())
    datatable(marco_grillas(), 
              options = list(pageLength = 10, 
                            scrollX = TRUE,
                            autoWidth = TRUE))
  })
  
  # Función para verificar la integridad de los marcos
  observeEvent(input$verificar_marcos_btn, {
    req(marco_celdas(), marco_grillas())
    
    tryCatch({
      # 1. Verificar locaciones sin celdas
      conteo_por_locacion <- marco_celdas() %>% 
        count(LOCACION) %>% 
        arrange(n)
      
      conteo_celdas_por_locacion(conteo_por_locacion)
      
      # Identificar locaciones sin celdas (si las hubiera)
      # Aquí asumimos que existe una lista completa de locaciones en algún lugar
      # Como no tenemos esa lista, solo mostramos el conteo
      
      # 2. Verificar celdas con menos de 3 grillas
      conteo_por_celda <- marco_grillas() %>% 
        count(COD_CELDA) %>% 
        arrange(n)
      
      conteo_grillas_por_celda(conteo_por_celda)
      
      # Identificar celdas con menos de 3 grillas
      celdas_pocas <- conteo_por_celda %>%
        filter(n < 3)
      
      # Guardar el dataframe completo con COD_CELDA y conteo
      celdas_con_pocas_grillas(celdas_pocas)
      
      # 3. Verificación cruzada entre marcos
      # Celdas en marco_celdas pero no en marco_grillas
      celdas_marco <- unique(marco_celdas()$COD_CELDA)
      celdas_grillas <- unique(marco_grillas()$COD_CELDA)
      
      celdas_solo_marco <- setdiff(celdas_marco, celdas_grillas)
      celdas_solo_en_marco_celdas(celdas_solo_marco)
      
      # Celdas en marco_grillas pero no en marco_celdas
      celdas_solo_grillas <- setdiff(celdas_grillas, celdas_marco)
      celdas_solo_en_marco_grillas(celdas_solo_grillas)
      
      # Mostrar notificación de éxito
      showNotification("Verificación completada", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error en la verificación:", e$message), type = "error")
    })
  })
  
  # Mostrar conteo de celdas por locación
  output$conteo_celdas_locacion <- renderDT({
    req(conteo_celdas_por_locacion())
    datatable(conteo_celdas_por_locacion(), 
              options = list(pageLength = 15, 
                            scrollX = TRUE,
                            autoWidth = TRUE))
  })
  
  # Mostrar locaciones sin celdas
  output$locaciones_sin_celdas <- renderPrint({
    req(conteo_celdas_por_locacion())
    
    # Identificar locaciones con 0 celdas (si las hubiera)
    locaciones_cero <- conteo_celdas_por_locacion() %>%
      filter(n == 0)
    
    if (nrow(locaciones_cero) > 0) {
      cat("Locaciones sin celdas:\n")
      print(locaciones_cero)
    } else {
      cat("Todas las locaciones tienen al menos una celda.")
    }
  })
  
  # Resumen de locaciones
  output$resumen_locaciones <- renderUI({
    req(conteo_celdas_por_locacion())
    
    # Estadísticas de locaciones
    total_locaciones <- nrow(conteo_celdas_por_locacion())
    min_celdas <- min(conteo_celdas_por_locacion()$n)
    max_celdas <- max(conteo_celdas_por_locacion()$n)
    promedio_celdas <- mean(conteo_celdas_por_locacion()$n)
    
    tagList(
      h4("Resumen"),
      p(paste("Total de locaciones:", total_locaciones)),
      p(paste("Mínimo de celdas por locación:", min_celdas)),
      p(paste("Máximo de celdas por locación:", max_celdas)),
      p(paste("Promedio de celdas por locación:", round(promedio_celdas, 2)))
    )
  })
  
  # Mostrar conteo de grillas por celda
  output$conteo_grillas_celda <- renderDT({
    req(conteo_grillas_por_celda())
    datatable(conteo_grillas_por_celda(), 
              options = list(pageLength = 15, 
                            scrollX = TRUE,
                            autoWidth = TRUE))
  })
  
  # Mostrar celdas con pocas grillas
  output$celdas_pocas_grillas <- renderPrint({
    req(conteo_grillas_por_celda())
    
    # Filtrar directamente las celdas con menos de 3 grillas
    celdas_pocas <- conteo_grillas_por_celda() %>%
      filter(n < 3)
    
    if (nrow(celdas_pocas) > 0) {
      cat("Celdas con menos de 3 grillas:\n")
      print(celdas_pocas)
    } else {
      cat("Todas las celdas tienen al menos 3 grillas.")
    }
  })
  
  # Resumen de grillas
  output$resumen_grillas <- renderUI({
    req(conteo_grillas_por_celda(), celdas_con_pocas_grillas())
    
    # Estadísticas de grillas
    total_celdas <- nrow(conteo_grillas_por_celda())
    celdas_problema <- nrow(celdas_con_pocas_grillas())
    min_grillas <- min(conteo_grillas_por_celda()$n)
    max_grillas <- max(conteo_grillas_por_celda()$n)
    promedio_grillas <- mean(conteo_grillas_por_celda()$n)
    
    tagList(
      h4("Resumen"),
      p(paste("Total de celdas en marco_grillas:", total_celdas)),
      p(paste("Celdas con menos de 3 grillas:", celdas_problema)),
      p(paste("Mínimo de grillas por celda:", min_grillas)),
      p(paste("Máximo de grillas por celda:", max_grillas)),
      p(paste("Promedio de grillas por celda:", round(promedio_grillas, 2))),
      
      if(celdas_problema > 0) {
        tags$div(style = "color: red; font-weight: bold;",
                 paste("ADVERTENCIA: Se encontraron", celdas_problema, "celdas con menos de 3 grillas."))
      } else {
        tags$div(style = "color: green; font-weight: bold;",
                 "Todas las celdas tienen al menos 3 grillas.")
      }
    )
  })
  
  # Mostrar celdas que están en marco_celdas pero no en marco_grillas
  output$celdas_no_en_grillas <- renderDT({
    req(marco_celdas(), celdas_solo_en_marco_celdas())
    
    if (length(celdas_solo_en_marco_celdas()) > 0) {
      celdas_filtradas <- marco_celdas() %>%
        filter(COD_CELDA %in% celdas_solo_en_marco_celdas())
      
      datatable(celdas_filtradas, 
                options = list(pageLength = 10, 
                              scrollX = TRUE,
                              autoWidth = TRUE))
    } else {
      datatable(data.frame(mensaje = "No hay celdas que estén solo en marco_celdas"), 
                options = list(dom = 't'))
    }
  })
  
  # Mostrar sugerencia para celdas en marco_celdas pero no en marco_grillas
  output$sugerencia_celdas_no_en_grillas <- renderUI({
    req(celdas_solo_en_marco_celdas())
    
    if (length(celdas_solo_en_marco_celdas()) > 0) {
      tags$div(style = "margin-top: 10px; padding: 10px; background-color: #fff3cd; border-left: 5px solid #ffc107; color: #856404;",
               tags$i(class = "fa fa-exclamation-triangle", style = "margin-right: 5px;"),
               "SUGERENCIA: Revisa el SHAPEFILE DE CELDAS, hay algunas celdas que se quedaron sin rejillas y olvidaron borrarlas en ese shapefile")
    }
  })
  
  # Mostrar celdas que están en marco_grillas pero no en marco_celdas
  output$celdas_no_en_marco <- renderDT({
    req(marco_grillas(), celdas_solo_en_marco_grillas())
    
    if (length(celdas_solo_en_marco_grillas()) > 0) {
      grillas_filtradas <- marco_grillas() %>%
        filter(COD_CELDA %in% celdas_solo_en_marco_grillas()) %>%
        distinct(COD_CELDA, .keep_all = TRUE)
      
      datatable(grillas_filtradas, 
                options = list(pageLength = 10, 
                              scrollX = TRUE,
                              autoWidth = TRUE))
    } else {
      datatable(data.frame(mensaje = "No hay celdas que estén solo en marco_grillas"), 
                options = list(dom = 't'))
    }
  })
  
  # Mostrar sugerencia para celdas en marco_grillas pero no en marco_celdas
  output$sugerencia_celdas_no_en_marco <- renderUI({
    req(celdas_solo_en_marco_grillas())
    
    if (length(celdas_solo_en_marco_grillas()) > 0) {
      tags$div(style = "margin-top: 10px; padding: 10px; background-color: #f8d7da; border-left: 5px solid #dc3545; color: #721c24;",
               tags$i(class = "fa fa-exclamation-circle", style = "margin-right: 5px;"),
               "SUGERENCIA: Revisa el SHAPEFILE DE CELDAS, hay algunas celdas que han sido borradas pero cuyas rejillas se han mantenido en el SHAPEFILE DE REJILLAS. Revisa si (1) debes recuperar la celda o (2) debes eliminar las rejillas")
    }
  })
  
  # Resumen de verificación cruzada
  output$resumen_verificacion_cruzada <- renderUI({
    req(celdas_solo_en_marco_celdas(), celdas_solo_en_marco_grillas())
    
    tagList(
      h4("Resumen de Verificación Cruzada"),
      p(paste("Celdas solo en marco_celdas:", length(celdas_solo_en_marco_celdas()))),
      p(paste("Celdas solo en marco_grillas:", length(celdas_solo_en_marco_grillas()))),
      
      if (length(celdas_solo_en_marco_celdas()) > 0 || length(celdas_solo_en_marco_grillas()) > 0) {
        tags$div(style = "color: red; font-weight: bold;",
                 "ADVERTENCIA: Se encontraron inconsistencias entre los marcos de celdas y grillas.")
      } else {
        tags$div(style = "color: green; font-weight: bold;",
                 "Los marcos de celdas y grillas son consistentes.")
      }
    )
  })
  
  # ============================================================================ #
  # FASE 3: CÁLCULO DEL TAMAÑO MUESTRAL                                        #
  # ============================================================================ #
  
  # Variables reactivas - Fase 3
  n_muestral <- reactiveVal(NULL)
  parametros_calculo <- reactiveVal(NULL)
  
  # Función para calcular el tamaño muestral
  observeEvent(input$calcular_n_btn, {
    req(marco_grillas())
    
    tryCatch({
      # Verificar si existen las variables necesarias
      if (!exists("base_tph_umbral_fil")) {
        # Si no existe, crear datos simulados para demostración
        base_tph_umbral_fil <<- data.frame(
          TPH = rnorm(100, mean = 500, sd = 100),
          locacion = sample(LETTERS[1:10], 100, replace = TRUE)
        )
        showNotification("Usando datos simulados para TPH", type = "warning")
      }
      
      if (!exists("DEFF_extended")) {
        DEFF_extended <<- 1.5  # Valor simulado
        showNotification("Usando valor simulado para DEFF_extended", type = "warning")
      }
      
      # Obtener valores de los inputs
      nivel_confianza <- input$nivel_confianza / 100  # Convertir a proporción
      Z <- qnorm(1 - (1 - nivel_confianza) / 2)  # Valor crítico de la distribución normal estándar
      TNR <- input$tasa_no_respuesta / 100  # Convertir a proporción
      
      # Calcular estadísticas de la variable TPH
      med <- mean(base_tph_umbral_fil$TPH, na.rm = TRUE)
      e <- (input$margen_error / 100) * med  # Margen de error como porcentaje de la media
      σ <- sd(base_tph_umbral_fil$TPH, na.rm = TRUE)  # Desviación estándar
      
      # Tamaño total de la población (número total de rejillas)
      N <- nrow(marco_grillas())
      
      # Cálculo del tamaño de muestra para poblaciones finitas (sin DEFF para muestreo monoetápico)
      n <- round(((N * Z ^ 2 * σ ^ 2) / (e ^ 2 * (N - 1) + Z ^ 2 * σ ^ 2)) * (1 / (1 - TNR)))
      
      # Guardar el resultado y los parámetros
      n_muestral(n)
      
      parametros <- list(
        nivel_confianza = nivel_confianza * 100,
        Z = Z,
        TNR = TNR * 100,
        media = med,
        margen_error = input$margen_error,
        error_absoluto = e,
        desviacion_estandar = σ,
        N = N,
        n = n
      )
      
      parametros_calculo(parametros)
      
      # Mostrar notificación de éxito
      showNotification("Tamaño muestral calculado exitosamente", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error al calcular el tamaño muestral:", e$message), type = "error")
    })
  })
  
  # Mostrar el resultado del cálculo del tamaño muestral
  output$resultado_n_muestral <- renderUI({
    req(n_muestral(), parametros_calculo())
    
    params <- parametros_calculo()
    
    # Calcular la cantidad de rejillas que representa la TNR
    rejillas_tnr <- floor(n_muestral() * (params$TNR / 100))
    
    tagList(
      tags$div(style = "font-size: 24px; margin-bottom: 20px;",
               "Tamaño muestral (n): ", tags$span(style = "font-weight: bold; color: #007bff;", n_muestral())),
      tags$div(style = "font-size: 18px; margin-bottom: 10px;",
               "Total de rejillas en el marco: ", tags$span(style = "font-weight: bold;", params$N)),
      tags$div(style = "font-size: 18px; margin-bottom: 10px;",
               "Rejillas que representan la TNR (", params$TNR, "%): ", 
               tags$span(style = "font-weight: bold; color: #dc3545;", rejillas_tnr))
    )
  })
  
  # Mostrar la fórmula utilizada con LaTeX
  output$formula_n_muestral <- renderUI({
    withMathJax(
      tags$div(
        "$$n = \\left( \\frac{N \\cdot Z^2 \\cdot \\sigma^2}{e^2 \\cdot (N-1) + Z^2 \\cdot \\sigma^2} \\right) \\cdot \\frac{1}{1-TNR}$$",
        tags$br(),
        tags$br(),
        "Donde:",
        tags$ul(
          tags$li("\\(n\\) = Tamaño de la muestra"),
          tags$li("\\(N\\) = Tamaño de la población (total de rejillas)"),
          tags$li("\\(Z\\) = Valor crítico de la distribución normal estándar (nivel de confianza)"),
          tags$li("\\(\\sigma\\) = Desviación estándar de la población"),
          tags$li("\\(e\\) = Margen de error"),
          tags$li("\\(TNR\\) = Tasa de no respuesta")
        )
      )
    )
  })
  
  # Mostrar los parámetros utilizados en el cálculo
  output$parametros_n_muestral <- renderPrint({
    req(parametros_calculo())
    
    params <- parametros_calculo()
    
    cat("PARÁMETROS UTILIZADOS EN EL CÁLCULO:\n\n")
    cat("Nivel de confianza: ", params$nivel_confianza, "%\n")
    cat("Valor Z: ", round(params$Z, 4), "\n")
    cat("Tasa de no respuesta (TNR): ", params$TNR, "%\n")
    cat("Media de TPH: ", round(params$media, 2), "\n")
    cat("Margen de error: ", params$margen_error, "% de la media\n")
    cat("Error absoluto: ", round(params$error_absoluto, 2), "\n")
    cat("Desviación estándar: ", round(params$desviacion_estandar, 2), "\n")
    cat("Tamaño de la población (N): ", params$N, " rejillas\n")
    cat("\n")
    cat("RESULTADO:\n")
    cat("Tamaño muestral (n): ", params$n, " rejillas\n")
  })
  
  # ============================================================================ #
  # FASE 4: MUESTREO MONOETÁPICO                                              #
  # ============================================================================ #
  
  # Valores reactivos para almacenar los resultados del muestreo
  datos_finales_df <- reactiveVal(NULL)
  resumen_muestreo <- reactiveVal(NULL)
  
  observeEvent(input$ejecutar_muestreo_btn, {
    req(marco_celdas(), marco_grillas(), n_muestral())
    
    showNotification("Iniciando Muestreo Monoetápico...", type = "message", duration = 5)
    
    tryCatch({
      
      # Establecer la semilla para la reproducibilidad
      set.seed(input$seed_muestreo)
      
      # Cargar datos reactivos a variables locales
      mc <- marco_celdas()
      mg <- marco_grillas()
      n <- n_muestral()
      
      # 1. PREPARACIÓN DE DATOS Y ASIGNACIÓN INICIAL
      # ----------------------------------------------------
      # Contar total de rejillas y celdas únicas por locación.
      # Esto es crucial para la asignación y para aplicar la restricción.
      info_locaciones <- mg %>%
        group_by(LOCACION) %>%
        summarise(
          total_rejillas = n(),
          total_celdas = n_distinct(COD_CELDA),
          .groups = 'drop'
        )
      
      # Asignación Proporcional de rejillas a muestrear por locación.
      # Se asegura un mínimo de 2 muestras por locación.
      # Si una locación tiene solo 1 celda, se le asigna 1 muestra.
      asignacion <- info_locaciones %>%
        mutate(
          n_asignado = ifelse(total_celdas < 2, 1, 2)
        )
      
      rejillas_restantes <- n - sum(asignacion$n_asignado)
      
      if (rejillas_restantes > 0) {
        asignacion <- asignacion %>%
          mutate(
            proporcion = total_rejillas / sum(total_rejillas),
            rejillas_adicionales = floor(proporcion * rejillas_restantes)
          )
        
        diferencia <- rejillas_restantes - sum(asignacion$rejillas_adicionales)
        
        if (diferencia > 0) {
          asignacion <- asignacion %>%
            arrange(desc((proporcion * rejillas_restantes) - floor(proporcion * rejillas_restantes))) %>%
            mutate(rejillas_adicionales = rejillas_adicionales + ifelse(row_number() <= diferencia, 1, 0)) %>%
            arrange(LOCACION)
        }
        
        asignacion <- asignacion %>% 
          mutate(n_asignado = n_asignado + rejillas_adicionales) %>%
          dplyr::select(-proporcion, -rejillas_adicionales)
      }
      
      
      # 2. MUESTREO INICIAL (PRIMERA PASADA)
      # ----------------------------------------------------
      rejillas_muestreadas_total <- list()
      celdas_ya_muestreadas <- c()
      remanente_total <- 0
      
      # Dataframe para manejar la asignación
      asignacion_df <- as.data.frame(asignacion)
      
      for(i in 1:nrow(asignacion_df)){
        loc <- asignacion_df[i, "LOCACION"]
        n_a_muestrear <- asignacion_df[i, "n_asignado"]
        celdas_disponibles <- asignacion_df[i, "total_celdas"]
        
        # Restricción: No se puede muestrear más rejillas que celdas disponibles.
        n_real_a_muestrear <- min(n_a_muestrear, celdas_disponibles)
        
        # Calcular el remanente si la asignación excede la cantidad de celdas
        remanente_actual <- n_a_muestrear - n_real_a_muestrear
        remanente_total <- remanente_total + remanente_actual
        
        if (n_real_a_muestrear > 0) {
          # Filtrar las unidades de la locación actual
          unidades_loc <- mg %>% filter(LOCACION == loc)
          
          # 1. Seleccionar aleatoriamente las CELDAS únicas de donde saldrán las muestras
          celdas_unicas_loc <- unique(unidades_loc$COD_CELDA)
          celdas_seleccionadas <- sample(celdas_unicas_loc, n_real_a_muestrear)
          
          # Guardar las celdas ya usadas para no volver a muestrearlas en el reparto del remanente
          celdas_ya_muestreadas <- c(celdas_ya_muestreadas, celdas_seleccionadas)
          
          # 2. Para cada celda seleccionada, muestrear UNA SOLA rejilla al azar
          rejillas_de_loc <- c()
          for (celda_sel in celdas_seleccionadas) {
            rejilla_unica_muestra <- unidades_loc %>%
              filter(COD_CELDA == celda_sel) %>%
              sample_n(1) %>%
              pull(COD_GRILLA)
            rejillas_de_loc <- c(rejillas_de_loc, rejilla_unica_muestra)
          }
          
          rejillas_muestreadas_total[[loc]] <- rejillas_de_loc
        }
      }
      
      
      # 3. REPARTO Y MUESTREO DEL REMANENTE (SEGUNDA PASADA)
      # ----------------------------------------------------
      if (remanente_total > 0) {
        showNotification(paste("Remanente de", remanente_total, "muestras a redistribuir..."), type = "message")
        
        # Identificar locaciones con celdas aún disponibles (que no han sido muestreadas)
        capacidad_restante <- mg %>%
          filter(!COD_CELDA %in% celdas_ya_muestreadas) %>%
          group_by(LOCACION) %>%
          summarise(celdas_libres = n_distinct(COD_CELDA), .groups = 'drop')
        
        if (nrow(capacidad_restante) > 0 && sum(capacidad_restante$celdas_libres) > 0) {
          
          # Limitar el remanente a la capacidad total disponible
          remanente_a_repartir <- min(remanente_total, sum(capacidad_restante$celdas_libres))
          
          # Asignación proporcional del remanente
          capacidad_restante <- capacidad_restante %>%
            mutate(
              proporcion_capacidad = celdas_libres / sum(celdas_libres),
              remanente_asignado = floor(proporcion_capacidad * remanente_a_repartir)
            )
          
          # Distribuir el sobrante del remanente
          sobrante_remanente <- remanente_a_repartir - sum(capacidad_restante$remanente_asignado)
          if (sobrante_remanente > 0) {
            capacidad_restante <- capacidad_restante %>%
              arrange(desc((proporcion_capacidad * remanente_a_repartir) - floor(proporcion_capacidad * remanente_a_repartir))) %>%
              mutate(remanente_asignado = remanente_asignado + ifelse(row_number() <= sobrante_remanente, 1, 0)) %>%
              arrange(LOCACION)
          }
          
          # Muestrear el remanente
          for (i in 1:nrow(capacidad_restante)) {
            loc_remanente <- capacidad_restante$LOCACION[i]
            n_remanente <- capacidad_restante$remanente_asignado[i]
            
            if (n_remanente > 0) {
              # Obtener celdas disponibles para esta locación
              celdas_disponibles_loc <- mg %>%
                filter(LOCACION == loc_remanente, !COD_CELDA %in% celdas_ya_muestreadas) %>%
                distinct(COD_CELDA) %>%
                pull(COD_CELDA)
              
              if (length(celdas_disponibles_loc) >= n_remanente) {
                # Seleccionar celdas para el remanente
                celdas_remanente <- sample(celdas_disponibles_loc, n_remanente)
                
                # Muestrear una rejilla por cada celda seleccionada
                rejillas_remanente <- c()
                for (celda_rem in celdas_remanente) {
                  rejilla_rem <- mg %>%
                    filter(LOCACION == loc_remanente, COD_CELDA == celda_rem) %>%
                    sample_n(1) %>%
                    pull(COD_GRILLA)
                  rejillas_remanente <- c(rejillas_remanente, rejilla_rem)
                }
                
                # Añadir al resultado total
                if (loc_remanente %in% names(rejillas_muestreadas_total)) {
                  rejillas_muestreadas_total[[loc_remanente]] <- c(rejillas_muestreadas_total[[loc_remanente]], rejillas_remanente)
                } else {
                  rejillas_muestreadas_total[[loc_remanente]] <- rejillas_remanente
                }
                
                # Actualizar celdas ya muestreadas
                celdas_ya_muestreadas <- c(celdas_ya_muestreadas, celdas_remanente)
              }
            }
          }
        }
      }
      
      # 4. PREPARACIÓN DE DATOS FINALES
      # ----------------------------------------------------
      nombres_rejillas_seleccionadas <- unlist(rejillas_muestreadas_total)
      
      # 5. VERIFICACIÓN DE MUESTRA
      datos_final <- mg %>% 
        filter(COD_GRILLA %in% nombres_rejillas_seleccionadas) %>%
        dplyr::select(LOCACION, COD_CELDA, COD_GRILLA, ESTE, NORTE, PROF, P_SUPERPOS)
      
      datos_finales_df(datos_final)
      
      # Generar resumen
      resumen_final <- capture.output({
        cat("Revisión final de n de rejillas y celdas en el excel\n")
        cat("-----------------------------------------------------\n")
        cat("Nº de locaciones únicas:", length(unique(datos_final$LOCACION)), "\n")
        cat("Nº de celdas únicas:", length(unique(datos_final$COD_CELDA)), "\n")
        cat("Nº de rejillas únicas (n final):", length(unique(datos_final$COD_GRILLA)), "\n\n")
        cat("Conteo de rejillas por locación:\n")
        print(datos_final %>% count(LOCACION) %>% arrange(n))
      })
      
      resumen_muestreo(paste(resumen_final, collapse = "\n"))
      
      showNotification("Muestreo Monoetápico completado exitosamente.", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error en el muestreo Monoetápico:", e$message), type = "error", duration = 10)
      resumen_muestreo(paste("Error:", e$message))
    })
  })
  
  # Lógica para generar distancias a pozos
  observeEvent(input$generar_distancias_btn, {
    req(input$archivo_pozos_referencia, datos_finales_df())
    
    showNotification("Cargando pozos de referencia y calculando distancias...", type = "message")
    
    tryCatch({
      # Leer el archivo Excel de pozos de referencia
      pozos_data <- read_excel(input$archivo_pozos_referencia$datapath)
      
      # Estandarizar nombres de columnas
      pozos_data <- estandarizar_columnas(pozos_data)
      
      # Verificar que existan las columnas requeridas
      verificar_columnas_requeridas(pozos_data, c("LOCACION", "ESTE", "NORTE", "ALTITUD"), "archivo de pozos de referencia")
      
      # Almacenar pozos de referencia
      pozos_referencia(pozos_data)
      
      # Obtener datos finales actuales
      datos_actuales <- datos_finales_df()
      
      # Calcular distancias y añadir altitudes
      datos_con_distancias <- añadir_distancias_pozos(datos_actuales, pozos_data)
      
      # Actualizar los datos finales con las nuevas columnas
      datos_finales_df(datos_con_distancias)
      datos_finales_con_distancias(datos_con_distancias)
      
      showNotification("Distancias y altitudes añadidas exitosamente a la muestra final.", type = "message")
      
    }, error = function(e) {
      registrar_error(e, "Generar distancias a pozos")
      showNotification(paste("Error al generar distancias:", conditionMessage(e)), type = "error")
    })
  })
  
  # Variables reactivas para las nuevas tablas
  estadisticas_generales <- reactive({
    req(datos_finales_df())
    datos_final <- datos_finales_df()
    
    data.frame(
      Estadística = c("Nº de locaciones únicas", "Nº de celdas únicas", "Nº de rejillas únicas (n final)"),
      Valor = c(
        length(unique(datos_final$LOCACION)),
        length(unique(datos_final$COD_CELDA)),
        length(unique(datos_final$COD_GRILLA))
      ),
      stringsAsFactors = FALSE
    )
  })
  
  conteo_rejillas_por_locacion <- reactive({
    req(datos_finales_df())
    datos_final <- datos_finales_df()
    
    datos_final %>% 
      count(LOCACION, name = "Número_de_Rejillas") %>% 
      arrange(Número_de_Rejillas)
  })
  
  # Mostrar tabla de estadísticas generales
  output$tabla_estadisticas_generales <- renderDT({
    req(estadisticas_generales())
    
    datatable(
      estadisticas_generales(),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 't',  # Solo mostrar la tabla, sin controles
        ordering = FALSE
      ),
      rownames = FALSE
    ) %>%
    formatStyle(
      "Valor",
      fontWeight = "bold",
      color = "#007bff"
    )
  })
  
  # Mostrar tabla de conteo de rejillas por locación
  output$tabla_conteo_rejillas_locacion <- renderDT({
    req(conteo_rejillas_por_locacion())
    
    datatable(
      conteo_rejillas_por_locacion(),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      rownames = FALSE
    ) %>%
    formatStyle(
      "Número_de_Rejillas",
      fontWeight = "bold"
    )
  })
  
  # Descargar tabla de conteo de rejillas por locación
  output$descargar_conteo_rejillas_btn <- downloadHandler(
    filename = function() {
      paste("Conteo_Rejillas_por_Locacion-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(conteo_rejillas_por_locacion())
      openxlsx::write.xlsx(conteo_rejillas_por_locacion(), file)
    }
  )
  
  # Mostrar tabla de muestra final
  output$tabla_muestra_final <- renderDT({
    req(datos_finales_df())
    
    datos <- datos_finales_df()
    
    dt <- datatable(
      datos,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(
          list(width = "300px", targets = which(names(datos) == "DISTANCIA") - 1)  # Hacer columna DISTANCIA más ancha
        )
      ),
      rownames = FALSE
    )
    
    # Aplicar formato especial si existen las columnas DISTANCIA y ALTITUD
    if ("DISTANCIA" %in% names(datos)) {
      dt <- dt %>% formatStyle(
        "DISTANCIA",
        fontSize = "12px",
        whiteSpace = "normal",
        wordWrap = "break-word"
      )
    }
    
    if ("ALTITUD" %in% names(datos)) {
      dt <- dt %>% formatStyle(
        "ALTITUD",
        fontWeight = "bold",
        color = "#28a745"
      )
    }
    
    return(dt)
  })
  
  # Función auxiliar para encontrar un "orden de visita" por vecino más cercano
  nearest_neighbor_order <- function(x, y) {
    n <- length(x)
    if (n == 0) return(integer(0))
    if (n == 1) return(1L)
    
    indices_disponibles <- seq_len(n)
    idx_inicial <- order(x, y)[1]
    
    orden <- integer(n)
    current_idx_in_disponibles <- which(indices_disponibles == idx_inicial)
    
    for (i in seq_len(n)) {
      original_idx <- indices_disponibles[current_idx_in_disponibles]
      orden[i] <- original_idx
      
      indices_disponibles <- indices_disponibles[-current_idx_in_disponibles]
      
      if (length(indices_disponibles) == 0) break
      
      distancias <- sqrt((x[original_idx] - x[indices_disponibles])^2 + 
                         (y[original_idx] - y[indices_disponibles])^2)
      
      current_idx_in_disponibles <- which.min(distancias)
    }
    
    return(orden)
  }

  # Lógica para añadir códigos de campo
  observeEvent(input$generar_codigos_btn, {
    req(datos_finales_df())

    if ("COD_PUNTO_CAMPO" %in% names(datos_finales_df())) {
      showNotification("Los códigos de campo ya han sido generados.", type = "warning")
      return()
    }

    showNotification("Generando códigos de campo y colectora...", type = "message", duration = 5)

    tryCatch({
      datosFINAL <- datos_finales_df()

      datosFINAL_result <- datosFINAL %>%
        group_by(LOCACION) %>%
        group_map(.f = function(df_loc, key_loc) {
          # A) dbscan para agrupar puntos
          clustering <- dbscan(as.matrix(df_loc[, c("ESTE", "NORTE")]), eps = 10, minPts = 1)
          df_loc$cluster_id <- clustering$cluster

          # B) Calcular centroides y su orden
          centroides <- df_loc %>%
            group_by(cluster_id) %>%
            summarize(cE = mean(ESTE), cN = mean(NORTE), .groups = "drop") %>%
            arrange(cE, cN) %>%
            mutate(cluster_orden = row_number())

          # C) Unir el orden del cluster
          df_loc <- df_loc %>% left_join(centroides %>% dplyr::select(cluster_id, cluster_orden), by = "cluster_id")

          # D) Ordenar puntos dentro de cada cluster
          df_loc_ordenado <- df_loc %>%
            group_by(cluster_orden) %>%
            group_modify(.f = function(dcluster, key_cl) {
              idx_orden_local <- nearest_neighbor_order(dcluster$ESTE, dcluster$NORTE)
              dcluster$orden_en_cluster <- seq_len(nrow(dcluster))[order(idx_orden_local)]
              return(dcluster)
            }) %>%
            ungroup() %>%
            arrange(cluster_orden, orden_en_cluster)

          # E) Número correlativo final y código
          df_loc_ordenado <- df_loc_ordenado %>%
            mutate(num_final = row_number(),
                   COD_GRILLA_NUMERADA_ESPACIALMENTE = paste0(key_loc$LOCACION, "-", num_final),
                   LOCACION = key_loc$LOCACION) # Re-añadir la columna de agrupación

          return(df_loc_ordenado)
        }) %>%
        bind_rows()

      # F) Crear códigos finales y seleccionar columnas
      datosFINAL_result <- datosFINAL_result %>%
        mutate(COD_PUNTO_CAMPO = paste0("L-X,6,PZ", COD_GRILLA_NUMERADA_ESPACIALMENTE),
               COD_COLECTORA = sub(".*PZ", "", COD_PUNTO_CAMPO))
      
      # Seleccionar columnas base y añadir DISTANCIA y ALTITUD si existen
      columnas_base <- c("LOCACION", "COD_CELDA", "COD_GRILLA", "ESTE", "NORTE", "PROF", "P_SUPERPOS", "COD_PUNTO_CAMPO", "COD_COLECTORA")
      columnas_adicionales <- c("DISTANCIA", "ALTITUD")
      columnas_existentes <- columnas_adicionales[columnas_adicionales %in% names(datosFINAL_result)]
      columnas_finales <- c(columnas_base, columnas_existentes)
      
      datosFINAL_result <- datosFINAL_result %>%
        dplyr::select(all_of(columnas_finales))

      datos_finales_df(datosFINAL_result)

      showNotification("Códigos generados y añadidos a la tabla.", type = "message")
    }, error = function(e) {
      registrar_error(e, "Generación de Códigos")
      showNotification(paste("Error al generar códigos:", conditionMessage(e)), type = "error")
    })
  })

  # Handlers para descargar tablas de verificación de marcos
  
  # 1. Conteo de Celdas por Locación
  output$download_conteo_celdas <- downloadHandler(
    filename = function() {
      paste("Conteo_Celdas_por_Locacion-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(conteo_celdas_por_locacion())
      openxlsx::write.xlsx(conteo_celdas_por_locacion(), file)
    }
  )
  
  # 2. Locaciones sin Celdas
  output$download_locaciones_sin_celdas <- downloadHandler(
    filename = function() {
      paste("Locaciones_sin_Celdas-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(conteo_celdas_por_locacion())
      # Identificar locaciones con 0 celdas (si las hubiera)
      locaciones_cero <- conteo_celdas_por_locacion() %>%
        filter(n == 0)
      
      if (nrow(locaciones_cero) > 0) {
        openxlsx::write.xlsx(locaciones_cero, file)
      } else {
        # Crear un dataframe con un mensaje si no hay locaciones sin celdas
        df_mensaje <- data.frame(mensaje = "Todas las locaciones tienen al menos una celda.")
        openxlsx::write.xlsx(df_mensaje, file)
      }
    }
  )
  
  # 3. Conteo de Grillas por Celda
  output$download_conteo_grillas <- downloadHandler(
    filename = function() {
      paste("Conteo_Grillas_por_Celda-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(conteo_grillas_por_celda())
      openxlsx::write.xlsx(conteo_grillas_por_celda(), file)
    }
  )
  
  # 4. Celdas con menos de 3 Grillas
  output$download_celdas_pocas_grillas <- downloadHandler(
    filename = function() {
      paste("Celdas_con_menos_de_3_Grillas-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(conteo_grillas_por_celda())
      # Filtrar directamente las celdas con menos de 3 grillas
      celdas_pocas <- conteo_grillas_por_celda() %>%
        filter(n < 3)
      
      if (nrow(celdas_pocas) > 0) {
        openxlsx::write.xlsx(celdas_pocas, file)
      } else {
        # Crear un dataframe con un mensaje si no hay celdas con pocas grillas
        df_mensaje <- data.frame(mensaje = "Todas las celdas tienen al menos 3 grillas.")
        openxlsx::write.xlsx(df_mensaje, file)
      }
    }
  )
  
  # 5. Celdas en marco_celdas pero no en marco_grillas
  output$download_celdas_no_en_grillas <- downloadHandler(
    filename = function() {
      paste("Celdas_en_marco_celdas_no_en_grillas-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(marco_celdas(), celdas_solo_en_marco_celdas())
      
      if (length(celdas_solo_en_marco_celdas()) > 0) {
        celdas_filtradas <- marco_celdas() %>%
          filter(COD_CELDA %in% celdas_solo_en_marco_celdas())
        openxlsx::write.xlsx(celdas_filtradas, file)
      } else {
        # Crear un dataframe con un mensaje si no hay celdas solo en marco_celdas
        df_mensaje <- data.frame(mensaje = "No hay celdas que estén solo en marco_celdas")
        openxlsx::write.xlsx(df_mensaje, file)
      }
    }
  )
  
  # 6. Celdas en marco_grillas pero no en marco_celdas
  output$download_celdas_no_en_marco <- downloadHandler(
    filename = function() {
      paste("Celdas_en_marco_grillas_no_en_celdas-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(marco_grillas(), celdas_solo_en_marco_grillas())
      
      if (length(celdas_solo_en_marco_grillas()) > 0) {
        grillas_filtradas <- marco_grillas() %>%
          filter(COD_CELDA %in% celdas_solo_en_marco_grillas()) %>%
          distinct(COD_CELDA, .keep_all = TRUE)
        openxlsx::write.xlsx(grillas_filtradas, file)
      } else {
        # Crear un dataframe con un mensaje si no hay celdas solo en marco_grillas
        df_mensaje <- data.frame(mensaje = "No hay celdas que estén solo en marco_grillas")
        openxlsx::write.xlsx(df_mensaje, file)
      }
    }
  )
  
  # Manejador de descarga para archivo Shapefile
  output$descargar_shp_btn <- downloadHandler(
    filename = function() {
      paste0("MuestraFinal_ConCodigos-", Sys.Date(), ".zip")
    },
    content = function(file) {
      req(datos_finales_df())
      
      showNotification("Preparando archivo Shapefile...", type = "message", duration = 5)
      
      tryCatch({
        # Asegurarse de que las columnas de coordenadas existan
        if (!all(c("ESTE", "NORTE") %in% names(datos_finales_df()))) {
          showNotification("Las columnas 'ESTE' y 'NORTE' son necesarias para el Shapefile.", type = "error")
          return(NULL)
        }

        # Convertir a objeto sf
        pts_sf <- st_as_sf(
          datos_finales_df(),
          coords = c("ESTE", "NORTE"),
          crs = 32717    # EPSG para WGS84 / UTM zona 17S
        )
        
        # Crear un directorio temporal para los archivos del shapefile
        temp_dir <- tempdir()
        shp_path <- file.path(temp_dir, "muestra_final.shp")
        
        # Escribir el shapefile
        st_write(
          obj = pts_sf,
          dsn = shp_path,
          delete_layer = TRUE # Sobrescribir si existe
        )
        
        # Listar todos los archivos componentes del shapefile
        files_to_zip <- list.files(temp_dir, pattern = "muestra_final\\..*", full.names = TRUE)
        
        # Comprimir los archivos en un .zip
        zip(zipfile = file, files = files_to_zip, flags = "-j") # -j para no guardar rutas
      }, error = function(e) {
        registrar_error(e$message, "Generación de Shapefile")
        showNotification(paste("Error al generar el Shapefile:", e$message), type = "error")
      })
    }
  )

  # Manejador de descarga para archivo Excel
  output$descargar_muestra_btn <- downloadHandler(
    filename = function() {
      paste("MuestraFinal_ConCodigos-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(datos_finales_df())
      # Usar write.xlsx para una exportación directa y simple
      openxlsx::write.xlsx(datos_finales_df(), file)
    }
  )

  output$parametros_n_muestral <- renderPrint({
    req(parametros_calculo())
    
    params <- parametros_calculo()
    
    cat("PARÁMETROS UTILIZADOS EN EL CÁLCULO:\n\n")
    cat("Nivel de confianza: ", params$nivel_confianza, "%\n")
    cat("Valor Z: ", round(params$Z, 4), "\n")
    cat("Tasa de no respuesta (TNR): ", params$TNR, "%\n")
    cat("Media de TPH: ", round(params$media, 2), "\n")
    cat("Margen de error: ", params$margen_error, "% de la media\n")
    cat("Error absoluto: ", round(params$error_absoluto, 2), "\n")
    cat("Desviación estándar: ", round(params$desviacion_estandar, 2), "\n")
    cat("Tamaño de la población (N): ", params$N, " rejillas\n")
    cat("Efecto de diseño (DEFF): ", round(params$DEFF, 4), "\n")
    cat("\n")
    cat("RESULTADO:\n")
    cat("Tamaño muestral (n): ", params$n, " rejillas\n")
  })
  
  # ============================================================================ #
  # SISTEMA DE ERRORES                                                          #
  # ============================================================================ #
  
  # Mostrar el registro de errores
  output$registro_errores <- renderPrint({
    errores <- registro_errores_lista()
    
    if (length(errores) == 0) {
      cat("No se han registrado errores.\n")
      cat("\n")
      cat("✅ La aplicación está funcionando correctamente.")
    } else {
      cat("REGISTRO DE ERRORES DE LA APLICACIÓN\n")
      cat("=====================================\n\n")
      
      # Mostrar errores en orden cronológico inverso (más recientes primero)
      for (i in length(errores):1) {
        error <- errores[[i]]
        cat(sprintf("[%s] %s\n", error$timestamp, 
                   if(error$contexto != "") paste0("(", error$contexto, ") ") else ""))
        cat(sprintf("Error: %s\n", error$mensaje))
        cat("---\n")
      }
      
      cat(sprintf("\nTotal de errores registrados: %d\n", length(errores)))
    }
  })
  
  # Limpiar el registro de errores
  observeEvent(input$limpiar_errores_btn, {
    registro_errores_lista(list())
    showNotification("Registro de errores limpiado.", type = "message")
  })
  
  # Descargar log de errores
  output$descargar_errores_btn <- downloadHandler(
    filename = function() {
      paste("log_errores_bietapico-", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      errores <- registro_errores_lista()
      
      if (length(errores) == 0) {
        writeLines("No se han registrado errores.", file)
      } else {
        lineas <- c(
          "REGISTRO DE ERRORES - APLICACIÓN MUESTREO BIETÁPICO",
          paste("Generado el:", Sys.time()),
          paste(rep("=", 60), collapse = ""),
          ""
        )
        
        for (i in 1:length(errores)) {
          error <- errores[[i]]
          lineas <- c(lineas,
                     sprintf("ERROR #%d", i),
                     sprintf("Fecha/Hora: %s", error$timestamp),
                     sprintf("Contexto: %s", if(error$contexto != "") error$contexto else "General"),
                     sprintf("Mensaje: %s", error$mensaje),
                     "",
                     paste(rep("-", 40), collapse = ""),
                     "")
        }
        
        lineas <- c(lineas, sprintf("Total de errores: %d", length(errores)))
        writeLines(lineas, file)
      }
    }
  )
}

# Correr la aplicación
shinyApp(ui = ui, server = server)
