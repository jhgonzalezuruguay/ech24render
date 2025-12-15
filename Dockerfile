FROM rocker/shiny:4.3.1

# Evitar preguntas interactivas
ENV DEBIAN_FRONTEND=noninteractive

# Instalar dependencias del sistema necesarias
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    && rm -rf /var/lib/apt/lists/*

# Instalar paquetes R requeridos por la app
RUN R -e "install.packages(c( \
  'shiny','data.table','dplyr','survey','tidyr','DT','plotly','scales', \
  'sf','ggplot2','geodata','stringi','leaflet','htmltools' \
), repos='https://cloud.r-project.org')"

# Copiar la app al contenedor
COPY app.R /srv/shiny-server/app.R

# Copiar configuración de Shiny Server
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# Exponer el puerto que usa Shiny
EXPOSE 3838

# Ejecutar Shiny Server
CMD ["/usr/bin/shiny-server"]

