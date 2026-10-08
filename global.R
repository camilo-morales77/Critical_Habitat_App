# ==============================================================================
# global.R -- Critical Habitat Assessment (CHA) App
# Package loading, app-wide options and IFC PS6 threshold configuration.
# ==============================================================================

required_packages <- c(
  "shiny", "shinydashboard", "leaflet", "sf", "terra", "dplyr", "DT", "readr", "writexl",
  "ggplot2", "png", "scales"
)

missing_pkgs <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_pkgs) > 0) {
  stop(sprintf(
    "Missing required packages: %s.\nInstall them with: install.packages(c(%s))",
    paste(missing_pkgs, collapse = ", "),
    paste(sprintf('"%s"', missing_pkgs), collapse = ", ")
  ))
}

# Optional: needed only for the Migratory Species panel (Cornell eBird
# Status & Trends). Not required for the app to launch -- the panel itself
# checks for it and shows an instructive message if missing.
# Install with: remotes::install_github("ebird/ebirdst")

library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(DT)
library(dplyr)

# Allow larger uploads (occurrence CSVs / shapefiles can be sizeable)
options(shiny.maxRequestSize = 300 * 1024^2)  # 300 MB

# CARTO basemap access used by every Leaflet panel.
CARTO_BASEMAP_API_KEY <- "cb1_3sbp_1_17309e37af7ae91f382b0939"

# ------------------------------------------------------------------------------
# IFC Performance Standard 6 / Guidance Note 6 (2019) -- Critical Habitat
# thresholds.
#
# Edit the `pct` values (and `label`) once confirmed against the official
# guidance note.
# ------------------------------------------------------------------------------
THRESHOLDS <- list(
  threatened = list(
    tier1 = list(label = "Tier 1 - Critically Endangered / Endangered species (higher significance)", pct = 0.5),
    tier2 = list(label = "Tier 2 - Vulnerable species / lower-confidence data (lower significance)", pct = 1)
  ),
  endemic = list(
    tier1 = list(label = "Critical Habitat - EAAA holds >=10% of the species' global distribution", pct = 10)
  ),
  ecosystems = list(
    tier1 = list(label = "Tier 1 - Critically Endangered / Endangered ecosystem (higher significance)", pct = 5)
  ),
  migratory = list(
    tier1 = list(label = "Tier 1 - Migratory/congregatory species, >=1% biogeographic population", pct = 1),
    tier2 = list(label = "Tier 2 - Migratory/congregatory species, >=0.5% biogeographic population", pct = 0.5)
  )
)
