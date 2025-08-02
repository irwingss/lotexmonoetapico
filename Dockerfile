# Usar una imagen base geoespacial que incluye dependencias para 'sf'
FROM rocker/geospatial:4.2.2

# Instalar dependencias del sistema
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgdal-dev \
    libudunits2-dev \
    libgeos-dev \
    libproj-dev \
    libsqlite3-dev \
    && rm -rf /var/lib/apt/lists/*

# Instalar paquetes R necesarios directamente
RUN R -e "install.packages(c(\
    'shiny', 'shinydashboard', 'readxl', 'DT', 'dplyr', \
    'TeachingSampling', 'dbscan', 'purrr', 'openxlsx', 'sf', \
    'colourpicker', 'uuid'), repos='https://cran.rstudio.com/')"

# Copiar la aplicación
COPY . /srv/shiny-server/app/
WORKDIR /srv/shiny-server/app

# Crear un script para iniciar la aplicación
RUN echo '#!/bin/sh\nR -e "shiny::runApp(\"/srv/shiny-server/app/app_01_muestreo_monoetapico.R\", host=\"0.0.0.0\", port=3838)"' > /usr/bin/start_app.sh \
    && chmod +x /usr/bin/start_app.sh

# Exponer el puerto de shiny
EXPOSE 3838

# Comando para iniciar la app
CMD ["/usr/bin/start_app.sh"]
