# ==============================================================================
# mod_ecosystems.R -- Threatened Ecosystems panel (Criterion 4)
# ==============================================================================

mod_ecosystems_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 4,
        box(
          title = "Threatened Ecosystem Data", status = "primary", solidHeader = TRUE, width = NULL,
          helpText("Upload the threatened/unique ecosystem layer to compare against the EAAA."),
          fileInput(
            ns("eco_file"), "Upload ecosystem layer", multiple = TRUE,
            accept = c(".shp", ".shx", ".dbf", ".prj", ".zip", ".gpkg", ".geojson", ".json")
          ),
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
          title = "Threatened Ecosystems - Results vs. IFC PS6 Thresholds",
          status = "warning", solidHeader = TRUE, width = NULL,
          DTOutput(ns("results_table")),
          table_download_button(ns),
          uiOutput(ns("verdict"))
        )
      )
    )
  )
}

mod_ecosystems_server <- function(id, eaaa, footprint, thresholds, study_crs) {
  moduleServer(id, function(input, output, session) {
    last_result <- reactiveVal(NULL)

    analysis <- eventReactive(input$analyze, {
      validate(need(!is.null(eaaa()), "Upload the EAAA in the Main panel first."))
      req(input$eco_file)
      poly <- read_vector_input(input$eco_file)
      crs_msg <- crs_status_message(poly)
      ov   <- calc_overlap(poly, eaaa(), study_crs())
      list(data = poly, overlap = ov, crs_msg = crs_msg)
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
      leafletProxy("map", session = session) %>%
        clearGroup("eco") %>%
        addPolygons(data = sf::st_transform(res$data, 4326), color = "#6a3d9a", fillOpacity = 0.2, group = "eco")
    })

    observe({
      res <- last_result()
      bb <- combined_bbox(eaaa(), footprint(), if (is.null(res)) NULL else res$data)
      if (!is.null(bb)) {
        leafletProxy("map", session = session) %>%
          fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
      }
    })

    observe({
      entries <- c()
      if (!is.null(eaaa())) entries <- c(entries, "EAAA" = "#e31a1c")
      if (!is.null(footprint())) entries <- c(entries, "Project footprint" = "#000000")
      if (!is.null(last_result())) entries <- c(entries, "Ecosystem layer" = "#6a3d9a")
      update_map_legend(session, "map", entries)
    })

    results_df <- reactive({
      req(analysis())
      ov <- analysis()$overlap
      build_threshold_table(ov$pct_of_eaaa, thresholds, area_ha = ov$area_overlap_km2 * 100)
    })

    output$results_table <- renderDT({
      style_threshold_table(
        DT::datatable(results_df(), rownames = FALSE, options = list(dom = "t")),
        results_df()
      )
    })

    output$download_table <- downloadHandler(
      filename = function() "CHA_threatened_ecosystems_results.xlsx",
      content = function(file) writexl::write_xlsx(results_df(), file)
    )

    output$download_map <- downloadHandler(
      filename = function() "CHA_threatened_ecosystems.png",
      content = function(file) {
        res <- last_result()
        bbox <- combined_bbox(eaaa(), footprint(), if (is.null(res)) NULL else res$data)
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
          if (!is.null(res)) {
            layers <- c(layers, list(list(data = res$data, label = "Ecosystem layer", color = "#6a3d9a", alpha = 0.2)))
          }
          p <- render_cha_static_map(bbox, layers)
        }
        ggplot2::ggsave(file, plot = p, width = 9, height = 6, dpi = 150, bg = "white")
      }
    )

    output$verdict <- renderUI({
      req(analysis())
      res <- analysis()
      tagList(
        tags$p(sprintf(
          "Threatened ecosystem overlap: %.1f%% of the ecosystem layer; %.1f%% of the EAAA.",
          res$overlap$pct_of_target, res$overlap$pct_of_eaaa
        )),
        tags$p(style = "color:#777; font-size:12px;", res$crs_msg)
      )
    })
  })
}
