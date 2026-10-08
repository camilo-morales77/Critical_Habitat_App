# ==============================================================================
# CHA_App.R -- Critical Habitat Assessment app entry point
#
# Run this app from the project folder with shiny::runApp().
# ==============================================================================
source("./global.R")
invisible(lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source))

ui <- dashboardPage(
  dashboardHeader(title = "Critical Habitat Assessment"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Main / Study Area", tabName = "main", icon = icon("map")),
      menuItem("Threatened Species", tabName = "threatened", icon = icon("paw")),
      menuItem("Endemic Species", tabName = "endemic", icon = icon("leaf")),
      menuItem("Threatened Ecosystems", tabName = "ecosystems", icon = icon("tree")),
      menuItem("Migratory Species", tabName = "migratory", icon = icon("dove"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "main", mod_main_ui("main")),
      tabItem(tabName = "threatened", mod_species_ui(
        "threatened", "Threatened Species (Criterion 1)", threatened = TRUE
      )),
      tabItem(tabName = "endemic", mod_species_ui("endemic", "Endemic / Restricted-Range Species (Criterion 2)")),
      tabItem(tabName = "ecosystems", mod_ecosystems_ui("ecosystems")),
      tabItem(tabName = "migratory", mod_migratory_ui("migratory"))
    )
  )
)

server <- function(input, output, session) {
  main_data <- mod_main_server("main")

  mod_species_server(
    "threatened", main_data$eaaa, main_data$footprint,
    THRESHOLDS$threatened, main_data$study_crs, threatened = TRUE
  )
  mod_species_server("endemic", main_data$eaaa, main_data$footprint, THRESHOLDS$endemic, main_data$study_crs)
  mod_ecosystems_server("ecosystems", main_data$eaaa, main_data$footprint, THRESHOLDS$ecosystems, main_data$study_crs)
  mod_migratory_server("migratory", main_data$eaaa, main_data$footprint, THRESHOLDS$migratory, main_data$study_crs)
}

shinyApp(ui, server)
