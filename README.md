# Aplicación Shiny de Diseño Monoetápico

Esta aplicación implementa un diseño de muestreo bietápico para análisis de datos espaciales. La aplicación permite cargar archivos Excel, realizar análisis de percentiles y visualizar resultados.

## Características

- Carga de archivos Excel con datos de celdas preliminares
- Análisis de percentiles y selección de umbrales
- Visualización de datos en tablas interactivas
- Exportación de resultados en formato Excel y Shapefile

## Requisitos

La aplicación requiere los siguientes paquetes de R:
- shiny
- shinydashboard
- readxl
- DT
- dplyr
- lme4
- performance
- TeachingSampling
- dbscan
- purrr
- openxlsx
- sf
- colourpicker
- uuid

## Ejecución Local

Para ejecutar la aplicación localmente:

```r
shiny::runApp("app_01_muestreo_monoetapico.R")
```

## Despliegue

Esta aplicación está configurada para desplegarse en Render usando Docker.
