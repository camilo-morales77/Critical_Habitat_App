# ==============================================================================
# mod_main.R -- Main panel: EAAA upload, preview map and CHA introduction
# ==============================================================================

mod_main_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 4,
        box(
          title = "Study Area", status = "primary", solidHeader = TRUE, width = NULL,
          helpText(
            "Upload the Ecologically Appropriate Area of Analysis (EAAA). ",
            "Accepted formats: shapefile (select the .shp/.shx/.dbf/.prj files ",
            "together, or a .zip containing them), GeoPackage (.gpkg), GeoJSON or KML."
          ),
          fileInput(
            ns("eaaa_file"), "Upload EAAA", multiple = TRUE,
            accept = c(".shp", ".shx", ".dbf", ".prj", ".zip", ".gpkg", ".geojson", ".json", ".kml")
          ),
          uiOutput(ns("eaaa_status")),
          tags$hr(),
          helpText(
            "Optional: upload the project's footprint for context. Any vector ",
            "geospatial format is accepted (shapefile components or .zip, ",
            "GeoPackage, GeoJSON, KML, and most other GDAL/sf-readable formats)."
          ),
          fileInput(
            ns("footprint_file"), "Upload project footprint (optional)", multiple = TRUE,
            accept = c(
              ".shp", ".shx", ".dbf", ".prj", ".zip", ".gpkg", ".geojson", ".json",
              ".kml", ".gml", ".tab", ".sqlite"
            )
          ),
          uiOutput(ns("footprint_status"))
        )
      ),
      column(
        width = 8,
        box(
          title = "Preview Map", status = "primary", solidHeader = TRUE, width = NULL,
          leafletOutput(ns("map"), height = 500),
          map_download_button(ns)
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        box(
          title = "About this Assessment", status = "info", solidHeader = TRUE, width = NULL,
          tags$p(
            "A Critical Habitat Assessment (CHA) evaluates whether a project's area of ",
            "influence overlaps areas of high biodiversity value, as defined under IFC ",
            "Performance Standard 6 (Biodiversity Conservation and Sustainable Management ",
            "of Living Natural Resources). Critical habitat is identified against five ",
            "criteria: (1) Critically Endangered / Endangered species, (2) endemic and/or ",
            "restricted-range species, (3) migratory and/or congregatory species, ",
            "(4) highly threatened and/or unique ecosystems, and (5) key evolutionary ",
            "processes."
          ),
          tags$p(
            "This app supports the GIS-based analysis of criteria 1-4 against the ",
            "Ecologically Appropriate Area of Analysis (EAAA) uploaded above. Criterion 5 ",
            "(evolutionary processes) is typically assessed through literature review ",
            "rather than spatial analysis and is out of scope here. Use the panels on the ",
            "left to analyze Threatened Species, Endemic Species, Threatened Ecosystems ",
            "and Migratory Species independently; each panel reuses the EAAA uploaded on ",
            "this page."
          )
        )
      )
    )
  )
}

mod_main_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    eaaa <- reactiveVal(NULL)
    footprint <- reactiveVal(NULL)
    study_crs <- reactiveVal(NULL)
    eaaa_crs_msg <- reactiveVal(NULL)
    footprint_crs_msg <- reactiveVal(NULL)

    observeEvent(input$eaaa_file, {
      req(input$eaaa_file)
      res <- tryCatch(read_vector_input(input$eaaa_file), error = function(e) e)
      if (inherits(res, "error")) {
        showNotification(paste("Error reading EAAA:", conditionMessage(res)), type = "error")
        return(invisible())
      }
      eaaa_crs_msg(crs_status_message(res))
      res <- sf::st_transform(sf::st_make_valid(res), 4326)
      eaaa(res)
      study_crs(pick_study_crs(res))
    })

    observeEvent(input$footprint_file, {
      req(input$footprint_file)
      res <- tryCatch(read_vector_input(input$footprint_file), error = function(e) e)
      if (inherits(res, "error")) {
        showNotification(paste("Error reading project footprint:", conditionMessage(res)), type = "error")
        return(invisible())
      }
      footprint_crs_msg(crs_status_message(res))
      res <- sf::st_transform(sf::st_make_valid(res), 4326)
      footprint(res)
    })

    output$eaaa_status <- renderUI({
      if (is.null(eaaa())) {
        tags$p(style = "color:#a94442;", "No EAAA uploaded yet.")
      } else {
        tagList(
          tags$p(style = "color:#3c763d;", sprintf("EAAA loaded (%d feature(s)).", nrow(eaaa()))),
          tags$p(style = "color:#777; font-size:12px;", eaaa_crs_msg()),
          tags$p(
            style = "color:#777; font-size:12px;",
            sprintf("All overlap/area calculations will use EPSG:%d (UTM) for this study.", study_crs())
          )
        )
      }
    })

    output$footprint_status <- renderUI({
      if (is.null(footprint())) {
        tags$p(style = "color:#777;", "No project footprint uploaded (optional).")
      } else {
        tagList(
          tags$p(style = "color:#3c763d;", sprintf("Project footprint loaded (%d feature(s)).", nrow(footprint()))),
          tags$p(style = "color:#777; font-size:12px;", footprint_crs_msg())
        )
      }
    })

    output$map <- renderLeaflet({
      cha_leaflet() %>%
        add_cha_basemap() %>%
        setView(lng = 0, lat = 0, zoom = 2)
    })
    outputOptions(output, "map", suspendWhenHidden = FALSE)

    observe({
      leafletProxy("map", session = session) %>% clearGroup("EAAA")
      if (!is.null(eaaa())) {
        leafletProxy("map", session = session) %>%
          addPolygons(data = eaaa(), color = "#e31a1c", weight = 2, fillOpacity = 0.15, group = "EAAA")
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

    observe({
      bb <- combined_bbox(eaaa(), footprint())
      if (!is.null(bb)) {
        leafletProxy("map", session = session) %>%
          fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
      }
    })

    observe({
      entries <- c()
      if (!is.null(eaaa())) entries <- c(entries, "EAAA" = "#e31a1c")
      if (!is.null(footprint())) entries <- c(entries, "Project footprint" = "#000000")
      update_map_legend(session, "map", entries)
    })

    output$download_map <- downloadHandler(
      filename = function() "CHA_study_area.png",
      content = function(file) {
        bbox <- combined_bbox(eaaa(), footprint())
        if (is.null(bbox)) {
          p <- ggplot2::ggplot() + ggplot2::theme_void() +
            ggplot2::annotate("text", x = 0, y = 0, label = "No layers uploaded yet.")
        } else {
          layers <- list()
          if (!is.null(eaaa())) layers <- c(layers, list(list(data = eaaa(), label = "EAAA", color = "#e31a1c", alpha = 0.15)))
          if (!is.null(footprint())) {
            layers <- c(layers, list(list(
              data = footprint(), label = "Project footprint", color = "#000000",
              alpha = 0.35, linetype = "dashed"
            )))
          }
          p <- render_cha_static_map(bbox, layers)
        }
        ggplot2::ggsave(file, plot = p, width = 9, height = 6, dpi = 150, bg = "white")
      }
    )

    list(eaaa = eaaa, footprint = footprint, study_crs = study_crs)
  })
}
