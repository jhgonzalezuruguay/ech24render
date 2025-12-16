FROM rocker/shiny:4.3.1

## ==== dependencias del sistema para sf / leaflet ====
RUN apt-get update && apt-get install -y \
  libgdal-dev \
  libgeos-dev \
  libproj-dev \
  libudunits2-dev \
  libssl-dev \
  libcurl4-openssl-dev \
  libxml2-dev \
  && rm -rf /var/lib/apt/lists/*

## ==== instalar paquetes R ====
RUN R -e "install.packages(c( \
  'data.table','dplyr','survey','tidyr','DT','plotly','scales', \
  'sf','ggplot2','geodata','stringi','leaflet','htmltools' \
), repos='https://cloud.r-project.org')"

## ==== copiar app ====
COPY app.R /srv/shiny-server/app.R

## ==== permisos ====
RUN chmod -R 755 /srv/shiny-server

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
