FROM rocker/shiny:4.3.1

ENV DEBIAN_FRONTEND=noninteractive

# =========================
# Dependencias del sistema (sf, leaflet, geodata)
# =========================
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    && rm -rf /var/lib/apt/lists/*

# =========================
# Paquetes de R requeridos por la app
# =========================
RUN R -e "install.packages(c( \
  'shiny', \
  'data.table', \
  'dplyr', \
  'survey', \
  'tidyr', \
  'DT', \
  'plotly', \
  'scales', \
  'sf', \
  'ggplot2', \
  'geodata', \
  'stringi', \
  'leaflet', \
  'htmltools' \
), repos='https://cloud.r-project.org')"

# =========================
# Copiamos la app y config
# =========================
COPY app.R /srv/shiny-server/app.R
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

RUN chown -R shiny:shiny /srv/shiny-server

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
