# ==============================================================================
# mod_species.R -- shared module for Threatened Species and Endemic Species
# panels (Criteria 1 and 2). Same UI/logic, different thresholds are passed in.
# ==============================================================================

mod_species_ui <- function(id, panel_title, threatened = FALSE) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 4,
        box(
          title = "Species Data", status = "primary", solidHeader = TRUE, width = NULL,
          textInput(ns("species_name"), "Species name (optional, for reference)"),
          if (threatened) selectInput(
            ns("threat_category"), "IUCN threat category",
            choices = c(
              "Critically Endangered (CR)" = "CR",
              "Endangered (EN)" = "EN",
              "Vulnerable (VU)" = "VU"
            )
          ),
          tags$strong("Occurrence points"),
          fileInput(
            ns("points_file"),
            "Upload occurrence records (CSV/TSV table, e.g. GBIF export, or a point shapefile)",
            multiple = TRUE,
            accept = c(".csv", ".tsv", ".shp", ".shx", ".dbf", ".prj", ".zip")
          ),
          uiOutput(ns("col_selectors")),
          tags$hr(),
          tags$strong("Distribution polygon"),
          fileInput(
            ns("poly_file"), "Upload distribution polygon", multiple = TRUE,
            accept = c(".shp", ".shx", ".dbf", ".prj", ".zip", ".gpkg", ".geojson", ".json")
          ),
          helpText("Upload either dataset or both. Each supplied dataset is analyzed separately."),
          actionButton(ns("analyze"), "Run analysis", icon = icon("play"), class = "btn-success")
        )
      ),
      column(
        width = 8,
        box(
          title = "Preview Map", status = "primary", solidHeader = TRUE, width = NULL,
          leafletOutput(ns("map"), height = 450),
          map_download_button(ns)
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        box(
          title = paste(panel_title, "- Results vs. IFC PS6 Thresholds"),
          status = "warning", solidHeader = TRUE, width = NULL,
          DTOutput(ns("results_table")),
          table_download_button(ns),
          uiOutput(ns("verdict"))
        )
      )
    )
  )
}

mod_species_server <- function(id, eaaa, footprint, thresholds, study_crs, threatened = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    last_result <- reactiveVal(NULL)

    raw_points <- reactive({
      req(input$points_file)
      req(is_tabular_points_upload(input$points_file))
      readr::read_csv(input$points_file$datapath[1], show_col_types = FALSE)
    })

    output$col_selectors <- renderUI({
      req(input$points_file)
      if (!is_tabular_points_upload(input$points_file)) return(NULL)
      req(raw_points())
      nms <- names(raw_points())
      guess_lon <- nms[grepl("^lon$|longitude|decimallongitude", nms, ignore.case = TRUE)][1]
      guess_lat <- nms[grepl("^lat$|latitude|decimallatitude", nms, ignore.case = TRUE)][1]
      tagList(
        selectInput(ns("lon_col"), "Longitude column", choices = nms, selected = guess_lon),
        selectInput(ns("lat_col"), "Latitude column", choices = nms, selected = guess_lat)
      )
    })

    analysis <- eventReactive(input$analyze, {
      validate(need(!is.null(eaaa()), "Upload the EAAA in the Main panel first."))
      has_points <- !is.null(input$points_file)
      has_polygon <- !is.null(input$poly_file)
      validate(need(has_points || has_polygon, "Upload occurrence points, a distribution polygon, or both."))

      result <- list(
        points = NULL, hull = NULL, point_counts = NULL, hull_overlap = NULL,
        polygon = NULL, polygon_overlap = NULL, polygon_crs_msg = NULL,
        threat_category = if (threatened) input$threat_category else NULL
      )

      if (has_points) {
        if (is_tabular_points_upload(input$points_file)) req(input$lon_col, input$lat_col)
        result$points <- read_points_file(input$points_file, input$lon_col, input$lat_col)
        result$point_counts <- points_in_polygon(result$points, eaaa())
        result$hull <- make_convex_hull(result$points)
        result$hull_overlap <- calc_overlap(result$hull, eaaa(), study_crs())
      }

      if (has_polygon) {
        result$polygon <- read_vector_input(input$poly_file)
        result$polygon_crs_msg <- crs_status_message(result$polygon)
        result$polygon_overlap <- calc_overlap(result$polygon, eaaa(), study_crs())
      }

      result
    })

    output$map <- renderLeaflet({
      cha_leaflet() %>% add_cha_basemap()
    })
    outputOptions(output, "map", suspendWhenHidden = FALSE)

    observe({
      leafletProxy("map", session = session) %>% clearGroup("EAAA")
      if (!is.null(eaaa())) {
        leafletProxy("map", session = session) %>%
          addPolygons(data = eaaa(), color = "#e31a1c", weight = 2, fillOpacity = 0.1, group = "EAAA")
      }
    })

    observe({
      leafletProxy("map", session = session) %>% clearGroup("footprint")
      if (!is.null(footprint())) {
        leafletProxy("map", session = session) %>%
          addPolygons(
            data = footprint(), color = "#000000", weight = 2, fillOpacity = 0.35,
            dashArray = "4", group = "footprint"
          )
      }
    })

    observeEvent(analysis(), {
      res <- analysis()
      last_result(res)
      proxy <- leafletProxy("map", session = session) %>% clearGroup("species")
      if (!is.null(res$points)) {
        proxy %>%
          addCircleMarkers(
            data = sf::st_transform(res$points, 4326), radius = 3, stroke = FALSE,
            fillOpacity = 0.8, color = "#1f78b4", group = "species"
          ) %>%
          addPolygons(
            data = sf::st_transform(res$hull, 4326), color = "#33a02c",
            fillOpacity = 0.05, dashArray = "4", group = "species"
          )
      }
      if (!is.null(res$polygon)) {
        proxy %>%
          addPolygons(data = sf::st_transform(res$polygon, 4326), color = "#ff7f00", fillOpacity = 0.2, group = "species")
      }
    })

    observe({
      res <- last_result()
      hull <- if (is.null(res)) NULL else res$hull
      polygon <- if (is.null(res)) NULL else res$polygon
      bb <- combined_bbox(eaaa(), footprint(), hull, polygon)
      if (!is.null(bb)) {
        leafletProxy("map", session = session) %>%
          fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
      }
    })

    observe({
      entries <- c()
      if (!is.null(eaaa())) entries <- c(entries, "EAAA" = "#e31a1c")
      if (!is.null(footprint())) entries <- c(entries, "Project footprint" = "#000000")
      res <- last_result()
      if (!is.null(res)) {
        if (!is.null(res$points)) {
          entries <- c(entries, "Occurrence points" = "#1f78b4", "Convex hull" = "#33a02c")
        }
        if (!is.null(res$polygon)) entries <- c(entries, "Distribution polygon" = "#ff7f00")
      }
      update_map_legend(session, "map", entries)
    })

    results_df <- reactive({
      req(analysis())
      res <- analysis()
      category_labels <- c(CR = "Critically Endangered (CR)", EN = "Endangered (EN)", VU = "Vulnerable (VU)")
      threshold_rows <- function(analysis_name, basis, pct, total = NA_integer_, inside = NA_integer_, area_ha = NA_real_) {
        result <- data.frame(
          Category = if (threatened) unname(category_labels[[res$threat_category]]) else NA_character_,
          Analysis = analysis_name,
          Basis = basis,
          `Total points` = total,
          `Inside EAAA` = inside,
          `Outside EAAA` = if (is.na(total)) NA_integer_ else total - inside,
          `Computed (%)` = round(pct, 2),
          `Overlap area (ha)` = round(area_ha, 2),
          check.names = FALSE
        )
        if (!threatened || res$threat_category != "VU") {
          applicable_thresholds <- if (threatened) thresholds["tier1"] else thresholds
          threshold_table <- build_threshold_table(pct, applicable_thresholds)
          result <- result[rep(1, nrow(threshold_table)), , drop = FALSE]
          result$Tier <- threshold_table$Tier
          result$`Threshold (%)` <- threshold_table$`Threshold (%)`
          result$Triggered <- threshold_table$Triggered
        }
        if (!threatened) result$Category <- NULL
        result
      }

      rows <- list()
      if (!is.null(res$points)) {
        rows[["points"]] <- threshold_rows(
          "Occurrence points", "% of occurrence points inside EAAA",
          res$point_counts$pct_inside, res$point_counts$n_total, res$point_counts$n_inside
        )
        rows[["hull"]] <- threshold_rows(
          "Occurrence convex hull", "% of convex hull (species range) overlapping EAAA",
          res$hull_overlap$pct_of_target, area_ha = res$hull_overlap$area_overlap_km2 * 100
        )
      }
      if (!is.null(res$polygon)) {
        rows[["polygon"]] <- threshold_rows(
          "Distribution polygon", "% of species range overlapping EAAA",
          res$polygon_overlap$pct_of_target, area_ha = res$polygon_overlap$area_overlap_km2 * 100
        )
      }
      do.call(rbind, rows)
    })

    output$results_table <- renderDT({
      style_threshold_table(
        DT::datatable(results_df(), rownames = FALSE, options = list(dom = "t", scrollX = TRUE)),
        results_df()
      )
    })

    output$download_table <- downloadHandler(
      filename = function() paste0("CHA_", id, "_species_results.xlsx"),
      content = function(file) writexl::write_xlsx(results_df(), file)
    )

    output$download_map <- downloadHandler(
      filename = function() paste0("CHA_", id, "_species.png"),
      content = function(file) {
        res <- last_result()
        hull <- if (is.null(res)) NULL else res$hull
        polygon <- if (is.null(res)) NULL else res$polygon
        bbox <- combined_bbox(eaaa(), footprint(), hull, polygon)
        if (is.null(bbox)) {
          p <- ggplot2::ggplot() + ggplot2::theme_void() +
            ggplot2::annotate("text", x = 0, y = 0, label = "No layers uploaded yet.")
        } else {
          layers <- list()
          if (!is.null(eaaa())) layers <- c(layers, list(list(data = eaaa(), label = "EAAA", color = "#e31a1c", alpha = 0.1)))
          if (!is.null(footprint())) {
            layers <- c(layers, list(list(
              data = footprint(), label = "Project footprint", color = "#000000",
              alpha = 0.35, linetype = "dashed"
            )))
          }
          if (!is.null(res$points)) {
            layers <- c(layers, list(
              list(data = res$points, label = "Occurrence points", color = "#1f78b4", geom = "point", size = 1.6),
              list(data = res$hull, label = "Convex hull", color = "#33a02c", alpha = 0.05, linetype = "dashed")
            ))
          }
          if (!is.null(res$polygon)) {
            layers <- c(layers, list(list(data = res$polygon, label = "Distribution polygon", color = "#ff7f00", alpha = 0.2)))
          }
          p <- render_cha_static_map(bbox, layers)
        }
        ggplot2::ggsave(file, plot = p, width = 9, height = 6, dpi = 150, bg = "white")
      }
    )


    output$verdict <- renderUI({
      req(analysis())
      res <- analysis()
      messages <- list()
      if (threatened) {
        messages[["category"]] <- tags$p(tags$strong(sprintf(
          "IUCN category analyzed: %s.",
          c(CR = "Critically Endangered (CR)", EN = "Endangered (EN)", VU = "Vulnerable (VU)")[[res$threat_category]]
        )))
      }
      if (!is.null(res$points)) {
        messages[["points"]] <- tags$p(sprintf(
          "Occurrence points inside EAAA: %d; outside EAAA: %d; total: %d (%.1f%% inside). Convex-hull overlap: %.1f%% of the hull area and %.1f%% of the EAAA.",
          res$point_counts$n_inside,
          res$point_counts$n_total - res$point_counts$n_inside,
          res$point_counts$n_total,
          res$point_counts$pct_inside,
          res$hull_overlap$pct_of_target,
          res$hull_overlap$pct_of_eaaa
        ))
      }
      if (!is.null(res$polygon)) {
        messages[["polygon"]] <- tags$p(sprintf(
          "Distribution polygon overlap with EAAA: %.1f%% of the species range and %.1f%% of the EAAA.",
          res$polygon_overlap$pct_of_target,
          res$polygon_overlap$pct_of_eaaa
        ))
        messages[["polygon_crs"]] <- tags$p(style = "color:#777; font-size:12px;", res$polygon_crs_msg)
      }
      if (threatened && res$threat_category == "VU") {
        messages[["vu_note"]] <- tags$p(
          tags$strong("PS6 orientation for Vulnerable species: "),
          "review whether the share of the species distribution within the EAAA, together with the likely project impact, could lead to the species being recategorized as Endangered or Critically Endangered. No automatic threshold is applied here."
        )
      }
      do.call(tagList, messages)
    })
  })
}
