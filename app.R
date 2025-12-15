# app.R
# Hogares MiPyME dependientes según ingreso principal (ECH 2024)
# Autor: José González Gómez

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
library(geodata)     # GADM URY level 1
library(stringi)
library(leaflet)
library(htmltools)

options(survey.lonely.psu = "adjust")

# === Descarga ECH 2024 desde Google Drive (Render compatible) ===
csv_path <- "ECH_2024.csv"

if (!file.exists(csv_path)) {
  download.file(
    url = "https://drive.google.com/uc?export=download&id=1wrZJ1K_mB4DtlJrlD28BDabpcG2OCtDj",
    destfile = csv_path,
    mode = "wb",
    quiet = FALSE
  )
}




# Ruta local del CSV (ajústala a tu PC)
ruta_csv <- csv_path


ui <- fluidPage(
  titlePanel("ECH 2024 — Hogares MiPyME dependientes (ingreso principal)"),
  sidebarLayout(
    sidebarPanel(
      checkboxInput("filtrar_ocupados", "Filtrar a hogares con al menos un ocupado", TRUE),
      selectInput("var_ing_hogar", "Variable de ingreso (hogar)", choices = c("ht11","ysvl"), selected = "ht11"),
      helpText("Tablas dinámicas (DT), gráficos interactivos (plotly) y mapa choropleth por departamento.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Resumen nacional",
                 h4("Proporción nacional de hogares MiPyME-dependientes"),
                 DTOutput("tabla_prop_nacional"),
                 br(),
                 h4("Proporción por departamento"),
                 DTOutput("tabla_dep_prop"),
                 br(),
                 plotlyOutput("plot_dep_prop")),
        tabPanel("Ingresos por hogar",
                 h4("Medias ponderadas por dependencia MiPyME (HT11, YSVL)"),
                 DTOutput("tabla_ing_hogar"),
                 br(),
                 plotlyOutput("plot_hogar_barras")),
        tabPanel("Ingresos personales (ocupados)",
                 h4("Medias entre ocupados por grupo MiPyME/No MiPyME"),
                 DTOutput("tabla_ing_persona"),
                 br(),
                 plotlyOutput("plot_persona_barras")),
        tabPanel("Participación vs país",
                 h4("Participación departamental en el total nacional de hogares dependientes"),
                 DTOutput("tabla_dep_vs_pais"),
                 br(),
                 plotlyOutput("plot_dep_vs_pais")),
        tabPanel("Mapa interactivo",
                 h4("Proporción de hogares MiPyME-dependientes por departamento"),
                 leafletOutput("mapa_leaflet", height = "600px"),
                 br(),
                 DTOutput("tabla_union_mapa"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Geometría departamental URY (GADM nivel 1) — reproducible y estándar
  uruguay_sf <- reactive({
    ur_raw <- geodata::gadm(country = "URY", level = 1, path = tempdir())
    st_as_sf(ur_raw) %>% 
      mutate(NAME_NORM = stri_trans_general(NAME_1, "Latin-ASCII") |> toupper() |> trimws())
  })
  
  # Carga y procesamiento de la base ECH
  base <- reactive({
    req(file.exists(ruta_csv))
    DT_full <- fread(ruta_csv, encoding = "UTF-8")
    
    # Armonizar nombres clave
    if ("F73" %in% names(DT_full) && !"f73" %in% names(DT_full)) setnames(DT_full, "F73", "f73")
    if ("F77" %in% names(DT_full) && !"f77" %in% names(DT_full)) setnames(DT_full, "F77", "f77")
    if ("G126_1" %in% names(DT_full) && !"g126_1" %in% names(DT_full)) setnames(DT_full, "G126_1", "g126_1")
    if (!"f73" %in% names(DT_full)) DT_full[, f73 := NA_integer_]
    if (!"f77" %in% names(DT_full)) DT_full[, f77 := NA_integer_]
    if (!"g126_1" %in% names(DT_full)) DT_full[, g126_1 := NA_real_]
    DT_full[, `:=`(f73 = as.integer(f73), f77 = as.integer(f77), g126_1 = as.numeric(g126_1))]
    
    # Indicadores MiPyME
    ocupado_vals <- c(1,2,3,4,7,8,9)
    mipyme_tam <- c(1,2,3,4,5,7)
    DT_full[, persona_trabaja := fifelse(!is.na(f73) & f73 %in% ocupado_vals, 1L, 0L)]
    DT_full[, empresa_mipyme := fifelse(!is.na(f77) & f77 %in% mipyme_tam, 1L, 0L)]
    DT_full[, persona_mipyme := fifelse(persona_trabaja == 1L & empresa_mipyme == 1L, 1L, 0L)]
    
    # Asegurar ID
    if (!"ID" %in% names(DT_full)) {
      keys <- intersect(c("anio","mes","numero","hogar"), names(DT_full))
      validate(need(length(keys) > 0, "Falta 'ID' y claves mínimas (anio, mes, numero, hogar)."))
      DT_full[, ID := .GRP, by = keys]
    } else {
      DT_full[, ID := as.integer(ID)]
    }
    
    # Filtrar hogares con al menos un ocupado
    if (isTRUE(input$filtrar_ocupados)) {
      DT_hog <- DT_full[, .(tiene_ocupado = any(f73 %in% ocupado_vals, na.rm = TRUE)), by = ID]
      DT_full <- merge(DT_full, DT_hog, by = "ID", all.x = TRUE)
      DT_full <- DT_full[tiene_ocupado == TRUE]
    }
    
    # Agregación a nivel hogar
    hogar_full <- DT_full[, .(
      ingreso_total_hogar = sum(g126_1, na.rm = TRUE),
      ingreso_mipyme_hogar = sum(fifelse(persona_mipyme == 1L, g126_1, 0), na.rm = TRUE),
      n_personas_hogar = .N,
      ht11 = if ("HT11" %in% names(DT_full)) suppressWarnings(first(na.omit(HT11))) else NA_real_,
      ysvl = if ("YSVL" %in% names(DT_full)) suppressWarnings(first(na.omit(YSVL))) else NA_real_
    ), by = ID]
    
    # Info diseño muestral y dpto
    hogar_info <- DT_full[, .(
      nom_dpto = if ("nom_dpto" %in% names(DT_full)) first(nom_dpto) else NA_character_,
      dpto = if ("dpto" %in% names(DT_full)) first(dpto) else NA,
      W_ANO = first(W_ANO),
      psu = if ("psu" %in% names(DT_full)) first(psu) else if ("congl" %in% names(DT_full)) first(congl) else NA,
      strata = if ("estrato" %in% names(DT_full)) first(estrato) else if ("ESTRED13" %in% names(DT_full)) first(ESTRED13) else NA
    ), by = ID]
    
    hogar_full <- merge(hogar_full, hogar_info, by = "ID", all.x = TRUE)
    hogar_full[, w := as.numeric(W_ANO)]
    hogar_full$ones <- 1
    hogar_full[, dpto_nombre := ifelse(!is.na(nom_dpto), nom_dpto, as.character(dpto))]
    
    # Indicador hogar MiPyME-dependiente
    hogar_full[, hogar_mipyme_dependiente := fifelse(ingreso_total_hogar > 0 &
                                                       ingreso_mipyme_hogar >= (ingreso_total_hogar * 0.5), 1L, 0L)]
    
    # Diseño muestral (hogar)
    validate(need(any(!is.na(hogar_full$w)), "No se encontró ponderador W_ANO válido."))
    has_psu <- "psu" %in% names(hogar_full) && any(!is.na(hogar_full$psu))
    has_strata <- "strata" %in% names(hogar_full) && any(!is.na(hogar_full$strata))
    svy_h <- if (has_psu && has_strata) {
      svydesign(ids = ~ psu, strata = ~ strata, weights = ~ w, data = hogar_full, nest = TRUE)
    } else if (has_psu) {
      svydesign(ids = ~ psu, weights = ~ w, data = hogar_full, nest = TRUE)
    } else {
      svydesign(ids = ~ 1, weights = ~ w, data = hogar_full)
    }
    
    # Proporción nacional
    prop_nac <- svymean(~hogar_mipyme_dependiente, design = svy_h, na.rm = TRUE)
    prop_nac_df <- data.frame(indicador = "hogar_mipyme_dependiente",
                              est = as.numeric(coef(prop_nac)),
                              se = as.numeric(SE(prop_nac)))
    
    # Proporción por departamento + total de hogares
    tab_dep_prop <- suppressWarnings(svyby(~hogar_mipyme_dependiente, ~dpto_nombre, svy_h, svymean, na.rm = TRUE))
    totales_dep <- suppressWarnings(svyby(~ones, ~dpto_nombre, svy_h, svytotal, na.rm = TRUE))
    dep_df <- data.frame(
      dpto = tab_dep_prop$dpto_nombre,
      prop_mipyme = as.numeric(coef(tab_dep_prop)),
      se_prop = as.numeric(SE(tab_dep_prop))
    ) %>%
      left_join(data.frame(dpto_nombre = totales_dep$dpto_nombre,
                           total_hog = as.numeric(coef(totales_dep))),
                by = c("dpto" = "dpto_nombre")) %>%
      arrange(desc(total_hog))
    
    # Medias ingreso por hogar
    inc_vars_h <- intersect(c("ht11","ysvl"), names(hogar_full))
    res_inc_h <- list()
    for (v in inc_vars_h) {
      mv <- try(svyby(as.formula(paste0("~", v)), ~hogar_mipyme_dependiente, svy_h, svymean, na.rm = TRUE), silent = TRUE)
      if (!inherits(mv, "try-error")) {
        res_inc_h[[v]] <- data.frame(d = mv$hogar_mipyme_dependiente,
                                     mean = as.numeric(coef(mv)),
                                     se = as.numeric(SE(mv)),
                                     variable = v)
      }
    }
    df_hog_medias <- bind_rows(res_inc_h) %>%
      mutate(grupo = ifelse(d == 1, "MiPyME dependiente", "No dependiente")) %>%
      select(variable, grupo, mean, se)
    
    # Medias personales (ocupados)
    DT_occ <- DT_full[persona_trabaja == 1]
    validate(need("W_ANO" %in% names(DT_occ), "Falta W_ANO en nivel persona."))
    DT_occ[, W_ANO := as.numeric(W_ANO)]
    has_psu_p <- "psu" %in% names(DT_occ) && any(!is.na(DT_occ$psu))
    strata_person <- if ("estrato" %in% names(DT_occ) && any(!is.na(DT_occ$estrato))) ~ estrato
    else if ("ESTRED13" %in% names(DT_occ) && any(!is.na(DT_occ$ESTRED13))) ~ ESTRED13
    else NULL
    svy_p_occ <- if (has_psu_p && !is.null(strata_person)) {
      svydesign(ids = ~ psu, strata = strata_person, weights = ~ W_ANO, data = DT_occ, nest = TRUE)
    } else if (has_psu_p) {
      svydesign(ids = ~ psu, weights = ~ W_ANO, data = DT_occ, nest = TRUE)
    } else {
      svydesign(ids = ~ 1, weights = ~ W_ANO, data = DT_occ)
    }
    
    inc_vars_p <- intersect(c("PT1","PT2","g126_1"), names(DT_occ))
    res_inc_p <- list()
    for (v in inc_vars_p) {
      mv <- try(svyby(as.formula(paste0("~", v)), ~persona_mipyme, svy_p_occ, svymean, na.rm = TRUE), silent = TRUE)
      if (!inherits(mv, "try-error")) {
        res_inc_p[[v]] <- data.frame(d = mv$persona_mipyme,
                                     mean = as.numeric(coef(mv)),
                                     se = as.numeric(SE(mv)),
                                     variable = v)
      }
    }
    df_persona_medias <- bind_rows(res_inc_p) %>%
      mutate(grupo = ifelse(d == 1, "Persona MiPyME (ocupado)", "Persona No MiPyME (ocupado)")) %>%
      select(variable, grupo, mean, se)
    
    # Participación dpto vs país
    hogares_mipyme_dep <- suppressWarnings(svyby(~hogar_mipyme_dependiente, ~dpto_nombre, svy_h, svytotal, na.rm = TRUE))
    hogares_mipyme_total <- suppressWarnings(svytotal(~hogar_mipyme_dependiente, svy_h, na.rm = TRUE))
    prop_dep_vs_pais <- data.frame(
      dpto = hogares_mipyme_dep$dpto_nombre,
      total_mipyme_dep = as.numeric(coef(hogares_mipyme_dep)),
      se_dep = as.numeric(SE(hogares_mipyme_dep)),
      total_mipyme_pais = as.numeric(coef(hogares_mipyme_total))
    ) %>%
      mutate(prop_vs_pais = total_mipyme_dep / total_mipyme_pais) %>%
      arrange(desc(prop_vs_pais))
    
    list(
      prop_nac_df = prop_nac_df,
      dep_df = dep_df,
      df_hog_medias = df_hog_medias,
      df_persona_medias = df_persona_medias,
      prop_dep_vs_pais = prop_dep_vs_pais
    )
  })
  
  # Tablas y gráficos
  output$tabla_prop_nacional <- renderDT({
    datatable(base()$prop_nac_df, rownames = FALSE,
              options = list(pageLength = 5, dom = "tip"),
              colnames = c("Indicador","Estimación","Error estándar")) %>%
      formatPercentage("est", 1) %>%
      formatRound("se", digits = 4)
  })
  
  output$tabla_dep_prop <- renderDT({
    datatable(base()$dep_df, rownames = FALSE,
              options = list(pageLength = 10, dom = "tip", order = list(list(3, 'desc'))),
              colnames = c("Departamento","Proporción MiPyME","SE","Total hogares")) %>%
      formatPercentage("prop_mipyme", 1) %>%
      formatRound("se_prop", 4) %>%
      formatRound("total_hog", 0)
  })
  
  output$plot_dep_prop <- renderPlotly({
    df <- base()$dep_df
    p <- ggplot(df, aes(x = reorder(dpto, prop_mipyme), y = prop_mipyme)) +
      geom_col(fill = "#2E7D32") +
      coord_flip() +
      scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
      labs(x = "Departamento", y = "Proporción MiPyME",
           title = "Hogares MiPyME-dependientes por departamento") +
      theme_minimal()
    ggplotly(p) %>% layout(hovermode = "closest")
  })
  
  output$tabla_ing_hogar <- renderDT({
    df <- base()$df_hog_medias %>% mutate(variable = toupper(variable))
    datatable(df, rownames = FALSE, options = list(pageLength = 5, dom = "tip"),
              colnames = c("Variable","Grupo","Media","SE")) %>% formatRound(c("mean","se"), 2)
  })
  
  output$plot_hogar_barras <- renderPlotly({
    df <- base()$df_hog_medias %>% filter(variable == input$var_ing_hogar)
    p <- ggplot(df, aes(x = grupo, y = mean, fill = grupo)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("No dependiente" = "steelblue", "MiPyME dependiente" = "tomato")) +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = "Ingreso medio (pesos)",
           title = paste("Ingreso medio por hogar —", toupper(input$var_ing_hogar))) +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(p) %>% layout(hovermode = "closest")
  })
  
  output$tabla_ing_persona <- renderDT({
    datatable(base()$df_persona_medias, rownames = FALSE,
              options = list(pageLength = 10, dom = "tip"),
              colnames = c("Variable","Grupo","Media","SE")) %>%
      formatRound(c("mean","se"), 2)
  })
  
  output$plot_persona_barras <- renderPlotly({
    df <- base()$df_persona_medias
    p <- ggplot(df, aes(x = variable, y = mean, fill = grupo)) +
      geom_col(position = position_dodge(width = 0.8)) +
      scale_y_continuous(labels = comma) +
      scale_fill_manual(values = c("Persona No MiPyME (ocupado)" = "steelblue",
                                   "Persona MiPyME (ocupado)" = "tomato")) +
      labs(x = "Variable de ingreso", y = "Ingreso medio (pesos)",
           title = "Ingresos medios entre ocupados: MiPyME vs No MiPyME") +
      theme_minimal()
    ggplotly(p) %>% layout(hovermode = "closest")
  })
  
  output$tabla_dep_vs_pais <- renderDT({
    datatable(base()$prop_dep_vs_pais, rownames = FALSE,
              options = list(pageLength = 10, dom = "tip"),
              colnames = c("Departamento","Total MiPyME (dep.)","SE","Total MiPyME (país)","Participación")) %>%
      formatRound(c("total_mipyme_dep","total_mipyme_pais"), 0) %>%
      formatRound("se_dep", 2) %>%
      formatPercentage("prop_vs_pais", 2)
  })
  
  output$plot_dep_vs_pais <- renderPlotly({
    df <- base()$prop_dep_vs_pais
    p <- ggplot(df, aes(x = reorder(dpto, prop_vs_pais), y = prop_vs_pais)) +
      geom_col(fill = "#1565C0") +
      coord_flip() +
      scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
      labs(x = "Departamento", y = "Participación en total país",
           title = "Participación departamental en hogares MiPyME-dependientes") +
      theme_minimal()
    ggplotly(p) %>% layout(hovermode = "closest")
  })
  
  # Mapa interactivo 
  
  output$mapa_leaflet <- renderLeaflet({
    
    uruy <- uruguay_sf() |>
      sf::st_make_valid() |>
      sf::st_cast("MULTIPOLYGON")
    
    dep_df <- base()$dep_df
    
    dep_df <- dep_df %>%
      mutate(
        DPTO_NORM = stri_trans_general(dpto, "Latin-ASCII") |>
          toupper() |>
          trimws()
      )
    
    dict <- c(
      "MONTEVIDEO" = "MONTEVIDEO",
      "CANELONES" = "CANELONES",
      "MALDONADO" = "MALDONADO",
      "COLONIA" = "COLONIA",
      "SAN JOSE" = "SAN JOSE",
      "SALTO" = "SALTO",
      "PAYSANDU" = "PAYSANDU",
      "RIO NEGRO" = "RIO NEGRO",
      "TACUAREMBO" = "TACUAREMBO",
      "CERRO LARGO" = "CERRO LARGO",
      "RIVERA" = "RIVERA",
      "SORIANO" = "SORIANO",
      "ROCHA" = "ROCHA",
      "ARTIGAS" = "ARTIGAS",
      "FLORIDA" = "FLORIDA",
      "LAVALLEJA" = "LAVALLEJA",
      "DURAZNO" = "DURAZNO",
      "TREINTA Y TRES" = "TREINTA Y TRES",
      "FLORES" = "FLORES"
    )
    
    dep_df$DPTO_NORM <- unname(dict[dep_df$DPTO_NORM])
    
    dep_df$DPTO_NORM[is.na(dep_df$DPTO_NORM)] <-
      dep_df$dpto[is.na(dep_df$DPTO_NORM)] |>
      stri_trans_general("Latin-ASCII") |>
      toupper() |>
      trimws()
    
    uruy_join <- uruy %>%
      left_join(dep_df, by = c("NAME_NORM" = "DPTO_NORM"))
    
    uruy_join$prop_mipyme[is.na(uruy_join$prop_mipyme)] <- 0
    uruy_join$total_hog[is.na(uruy_join$total_hog)] <- 0
    
    pal <- colorNumeric(
      palette = "viridis",
      domain = uruy_join$prop_mipyme
    )
    
    leaflet(uruy_join) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        fillColor = ~pal(prop_mipyme),
        weight = 1,
        color = "white",
        fillOpacity = 0.85,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#444",
          fillOpacity = 0.95,
          bringToFront = TRUE
        ),
        label = ~lapply(
          paste0(
            "<strong>", NAME_1, "</strong><br/>",
            "Proporción MiPyME: ",
            scales::percent(prop_mipyme, accuracy = 0.1), "<br/>",
            "Total hogares: ",
            scales::comma(total_hog)
          ),
          htmltools::HTML
        ),
        labelOptions = labelOptions(
          style = list(
            "font-weight" = "normal",
            padding = "6px"
          ),
          textsize = "13px",
          direction = "auto"
        )
      ) |>
      addLegend(
        pal = pal,
        values = ~prop_mipyme,
        title = "% Hogares MiPyME",
        opacity = 1
      )
  })
  
  
}

shinyApp(ui, server)