# ==============================================================================
# mod_migratory.R -- Migratory Species panel (Criterion 3)
# Uses Cornell's eBird Status & Trends ("ebirdst" package) abundance data to
# approximate the reproductive-unit (breeding season) distribution of a
# migratory species and compares it against the EAAA.
#
# NOTE: the `ebirdst` package requires a personal access key (request at
# https://science.ebird.org/en/status-and-trends/download-data) and downloads
# can be large. Function names differ slightly across ebirdst versions; this
# module tries the modern API first and falls back to the older one.
# ==============================================================================

mod_migratory_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 4,
        box(
          title = "Migratory Species Data (eBird Status & Trends)", status = "primary", solidHeader = TRUE, width = NULL,
          helpText(
            "Requires an eBird Status & Trends access key. Request one at ",
            tags$a(href = "https://science.ebird.org/en/status-and-trends/download-data", "ebird.org", target = "_blank"), "."
          ),
          passwordInput(ns("api_key"), "eBird Status & Trends access key"),
          textInput(ns("species_code"), "eBird species code (e.g. 'baleag' for Bald Eagle)"),
          selectInput(
            ns("season"), "Reproductive / seasonal period",
            choices = c(
              "Breeding" = "breeding",
              "Non-breeding" = "nonbreeding",
              "Pre-breeding migration" = "prebreeding_migration",
              "Post-breeding migration" = "postbreeding_migration"
            ),
            selected = "breeding"
          ),
          actionButton(ns("run"), "Download & Analyze", icon = icon("download"), class = "btn-success"),
          uiOutput(ns("status_msg"))
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
          title = "Migratory Species - Results vs. IFC PS6 Thresholds",
          status = "warning", solidHeader = TRUE, width = NULL,
          DTOutput(ns("results_table")),
          table_download_button(ns),
          uiOutput(ns("verdict"))
        )
      )
    )
  )
}

mod_migratory_server <- function(id, eaaa, footprint, thresholds, study_crs) {
  moduleServer(id, function(input, output, session) {
    status <- reactiveVal(NULL)
    last_result <- reactiveVal(NULL)
    output$status_msg <- renderUI({
      req(status())
      tags$p(style = "color:#31708f;", status())
    })

    download_abundance <- function(species_code) {
      # Try the modern ebirdst API first, then fall back to the legacy one.
      if (exists("ebirdst_download_status", where = asNamespace("ebirdst"))) {
        getExportedValue("ebirdst", "ebirdst_download_status")(
          species_code, pattern = "abundance_seasonal_mean_3km", show_progress = FALSE
        )
      } else {
        getExportedValue("ebirdst", "ebirdst_download")(
          species = species_code, pattern = "abundance_seasonal_mean_3km", show_progress = FALSE
        )
      }
    }

    analysis <- eventReactive(input$run, {
      validate(need(!is.null(eaaa()), "Upload the EAAA in the Main panel first."))
      validate(need(
        requireNamespace("ebirdst", quietly = TRUE),
        "The 'ebirdst' package is not installed. Install it with remotes::install_github('ebird/ebirdst')."
      ))
      validate(need(nzchar(input$api_key), "Enter your eBird Status & Trends access key."))
      validate(need(nzchar(input$species_code), "Enter an eBird species code."))

      status("Setting access key...")
      getExportedValue("ebirdst", "set_ebirdst_access_key")(
        input$api_key, overwrite = TRUE, quiet = TRUE
      )

      status("Downloading abundance data (first download for a species can take a while)...")
      path <- tryCatch(download_abundance(input$species_code), error = function(e) e)
      validate(need(!inherits(path, "error"), paste("Download failed:", if (inherits(path, "error")) conditionMessage(path) else "unknown error")))

      status("Processing raster...")
      r <- terra::rast(path)
      lyr_name <- grep(input$season, names(r), value = TRUE, ignore.case = TRUE)[1]
      validate(need(!is.na(lyr_name), sprintf("No '%s' season layer found for this species/period.", input$season)))
      r_season <- r[[lyr_name]]

      # Convert to a presence polygon (non-zero abundance = part of the
      # reproductive-unit distribution for that season).
      r_bin <- terra::classify(r_season, matrix(c(-Inf, 0, NA, 0, Inf, 1), ncol = 3, byrow = TRUE))
      poly  <- terra::as.polygons(r_bin, dissolve = TRUE)
      poly_sf <- sf::st_as_sf(poly)
      poly_sf <- sf::st_set_crs(poly_sf, terra::crs(r_bin))

      status("Calculating overlap with EAAA...")
      ov <- calc_overlap(poly_sf, eaaa(), study_crs())
      status("Done.")

      list(data = poly_sf, overlap = ov)
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
        clearGroup("migratory") %>%
        addPolygons(data = sf::st_transform(res$data, 4326), color = "#b15928", fillOpacity = 0.2, group = "migratory")
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
      if (!is.null(last_result())) entries <- c(entries, "Migratory distribution" = "#b15928")
      update_map_legend(session, "map", entries)
    })

    results_df <- reactive({
      req(analysis())
      ov <- analysis()$overlap
      build_threshold_table(ov$pct_of_target, thresholds, area_ha = ov$area_overlap_km2 * 100)
    })

    output$results_table <- renderDT({
      style_threshold_table(
        DT::datatable(results_df(), rownames = FALSE, options = list(dom = "t")),
        results_df()
      )
    })

    output$download_table <- downloadHandler(
      filename = function() "CHA_migratory_species_results.xlsx",
      content = function(file) writexl::write_xlsx(results_df(), file)
    )

    output$download_map <- downloadHandler(
      filename = function() "CHA_migratory_species.png",
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
            layers <- c(layers, list(list(data = res$data, label = "Migratory distribution", color = "#b15928", alpha = 0.2)))
          }
          p <- render_cha_static_map(bbox, layers)
        }
        ggplot2::ggsave(file, plot = p, width = 9, height = 6, dpi = 150, bg = "white")
      }
    )

    output$verdict <- renderUI({
      req(analysis())
      ov <- analysis()$overlap
      tags$p(sprintf(
        "Reproductive-unit (%s season) distribution overlap with EAAA: %.1f%% of the species' seasonal range; %.1f%% of the EAAA.",
        isolate(input$season), ov$pct_of_target, ov$pct_of_eaaa
      ))
    })
  })
}
