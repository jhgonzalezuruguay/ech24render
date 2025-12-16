# app.R
# Hogares MiPyME dependientes según ingreso principal (ECH 2024)
# Autor: José González Gómez

# =========================
# CONFIGURACIÓN INICIAL
# =========================

options(
  shiny.error = function() {
    traceback(2)
    q(status = 1)
  },
  survey.lonely.psu = "adjust"
)

Sys.setenv(SF_USE_S2 = "false")

cat("=== INICIANDO app.R ===\n")

# =========================
# LIBRERÍAS
# =========================

library(shiny)
library(data.table)
library(dplyr)
library(survey)
library(tidyr)
library(DT)
library(plotly)
library(scales)
library(sf)
library(ggplot2)
library(geodata)
library(stringi)
library(leaflet)
library(htmltools)

# =========================
# CARGA SHAPEFILE (GLOBAL)
# =========================

cat("Cargando shapefile Uruguay...\n")

ur_raw <- geodata::gadm(
  country = "URY",
  level = 1,
  path = tempdir()
)

URUGUAY_SF <- st_as_sf(ur_raw) |>
  mutate(
    NAME_NORM = stri_trans_general(NAME_1, "Latin-ASCII") |>
      toupper() |>
      trimws()
  )

cat("Shapefile Uruguay cargado OK\n")

# =========================
# UI
# =========================

ui <- fluidPage(
  titlePanel("ECH 2024 — Hogares MiPyME dependientes"),
  sidebarLayout(
    sidebarPanel(
      checkboxInput("filtrar_ocupados", "Filtrar hogares con ocupados", TRUE),
      selectInput(
        "var_ing_hogar",
        "Variable de ingreso hogar",
        choices = c("ht11", "ysvl"),
        selected = "ht11"
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Resumen",
                 DTOutput("tabla_prop_nacional"),
                 plotlyOutput("plot_dep_prop")),
        tabPanel("Mapa",
                 leafletOutput("mapa_leaflet", height = 600))
      )
    )
  )
)

# =========================
# SERVER
# =========================

server <- function(input, output, session) {

  # ---------- CSV ----------
  csv_path <- "ECH_2024.csv"

  if (!file.exists(csv_path)) {
    cat("Descargando ECH_2024.csv...\n")
    download.file(
      url = "https://drive.google.com/uc?export=download&id=1wrZJ1K_mB4DtlJrlD28BDabpcG2OCtDj",
      destfile = csv_path,
      mode = "wb",
      quiet = FALSE
    )
  }

  # ---------- BASE ----------
  base <- reactive({

    req(file.exists(csv_path))

    DT <- fread(csv_path, encoding = "UTF-8")

    # Nombres estándar
    setnames(DT, old = names(DT), new = tolower(names(DT)))

    # Variables mínimas
    DT[, f73 := as.integer(f73)]
    DT[, f77 := as.integer(f77)]
    DT[, g126_1 := as.numeric(g126_1)]

    ocupado_vals <- c(1,2,3,4,7,8,9)
    mipyme_tam   <- c(1,2,3,4,5,7)

    DT[, trabaja := f73 %in% ocupado_vals]
    DT[, mipyme  := trabaja & f77 %in% mipyme_tam]

    # ID hogar
    DT[, id := .GRP, by = .(anio, mes, numero, hogar)]

    if (isTRUE(input$filtrar_ocupados)) {
      DT <- DT[trabaja == TRUE]
    }

    hogar <- DT[, .(
      ingreso_total = sum(g126_1, na.rm = TRUE),
      ingreso_mipyme = sum(ifelse(mipyme, g126_1, 0), na.rm = TRUE),
      w = first(w_ano),
      dpto = first(nom_dpto)
    ), by = id]

    hogar[, dependiente := ingreso_total > 0 &
            ingreso_mipyme >= 0.5 * ingreso_total]

    diseño <- svydesign(
      ids = ~1,
      weights = ~w,
      data = hogar
    )

    prop_nac <- svymean(~dependiente, diseño, na.rm = TRUE)

    dep_df <- svyby(
      ~dependiente,
      ~dpto,
      diseño,
      svymean,
      na.rm = TRUE
    ) |>
      as.data.frame() |>
      rename(prop_mipyme = dependiente)

    list(
      prop_nac = data.frame(
        indicador = "Hogares MiPyME dependientes",
        valor = coef(prop_nac)
      ),
      dep_df = dep_df
    )
  })

  # ---------- OUTPUTS ----------

  output$tabla_prop_nacional <- renderDT({
    datatable(
      base()$prop_nac,
      rownames = FALSE
    ) |> formatPercentage("valor", 1)
  })

  output$plot_dep_prop <- renderPlotly({
    gg <- ggplot(base()$dep_df,
                 aes(x = reorder(dpto, prop_mipyme),
                     y = prop_mipyme)) +
      geom_col(fill = "#2E7D32") +
      coord_flip() +
      scale_y_continuous(labels = percent_format()) +
      theme_minimal()

    ggplotly(gg)
  })

  output$mapa_leaflet <- renderLeaflet({

    dep <- base()$dep_df |>
      mutate(
        DPTO_NORM = stri_trans_general(dpto, "Latin-ASCII") |>
          toupper() |>
          trimws()
      )

    mapa <- URUGUAY_SF |>
      left_join(dep, by = c("NAME_NORM" = "DPTO_NORM"))

    pal <- colorNumeric("viridis", mapa$prop_mipyme)

    leaflet(mapa) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        fillColor = ~pal(prop_mipyme),
        fillOpacity = 0.8,
        color = "white",
        weight = 1,
        label = ~paste0(
          NAME_1, ": ",
          percent(prop_mipyme, 0.1)
        )
      ) |>
      addLegend(
        pal = pal,
        values = ~prop_mipyme,
        title = "% Hogares MiPyME"
      )
  })
}

shinyApp(ui, server)
