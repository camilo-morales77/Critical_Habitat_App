# ==============================================================================
# utils_spatial.R -- shared spatial helpers for the CHA app
# ==============================================================================

#' Create a Leaflet map without pane animations that can misalign PNG captures.
cha_leaflet <- function(...) {
  leaflet::leaflet(options = leaflet::leafletOptions(
    zoomAnimation = FALSE, fadeAnimation = FALSE, markerZoomAnimation = FALSE
  ), ...)
}

#' Add the app's authenticated CARTO Positron basemap.
add_cha_basemap <- function(map) {
  leaflet::addTiles(
    map,
    urlTemplate = paste0(
      "https://basemaps.cartocdn.com/rastertiles/light_all/{z}/{x}/{y}.png",
      "?key=", CARTO_BASEMAP_API_KEY
    ),
    attribution = paste(
      "&copy; <a href='https://www.openstreetmap.org/copyright'>OpenStreetMap</a> contributors",
      "&copy; <a href='https://carto.com/attributions'>CARTO</a>"
    ),
    options = leaflet::tileOptions(
      maxZoom = 20, detectRetina = FALSE, crossOrigin = "anonymous"
    )
  )
}

#' Add a button that downloads a results table as an Excel (.xlsx) file.
table_download_button <- function(ns, output_id = "download_table") {
  shiny::downloadButton(
    ns(output_id), "Download table (Excel)",
    icon = shiny::icon("file-excel"), class = "btn-default",
    style = "margin-top: 10px;"
  )
}

# ------------------------------------------------------------------------------
# Static (ggplot2-based) map rendering for the "Download map (PNG)" buttons.
# Rendered server-side from the same CARTO tiles as the live Leaflet widget,
# instead of screen-capturing the browser DOM, so the basemap and the
# uploaded/analysis layers can never be captured out of alignment.
# ------------------------------------------------------------------------------

# In-memory cache of downloaded basemap tiles (keyed "zoom/x/y"), shared for
# the life of the R process; capped so a long-running session can't grow it
# without bound.
.cha_tile_cache <- new.env(parent = emptyenv())

cha_lon_to_tile_x <- function(lon, zoom) floor((lon + 180) / 360 * 2^zoom)
cha_lat_to_tile_y <- function(lat, zoom) {
  lat_rad <- lat * pi / 180
  floor((1 - log(tan(lat_rad) + 1 / cos(lat_rad)) / pi) / 2 * 2^zoom)
}
cha_tile_x_to_lon <- function(x, zoom) x / 2^zoom * 360 - 180
cha_tile_y_to_lat <- function(y, zoom) {
  n <- pi - 2 * pi * y / 2^zoom
  atan(sinh(n)) * 180 / pi
}

#' Download (or reuse from the in-process cache) a single CARTO basemap tile
#' as a 256x256x3 RGB array in [0, 1]. Falls back to a blank white tile if the
#' download fails, so one bad tile can't abort the whole export.
cha_fetch_tile <- function(zoom, x, y) {
  key <- sprintf("%d/%d/%d", zoom, x, y)
  cached <- get0(key, envir = .cha_tile_cache)
  if (!is.null(cached)) return(cached)

  if (length(ls(.cha_tile_cache)) > 500) rm(list = ls(.cha_tile_cache), envir = .cha_tile_cache)

  url <- sprintf(
    "https://basemaps.cartocdn.com/rastertiles/light_all/%d/%d/%d.png?key=%s",
    zoom, x, y, CARTO_BASEMAP_API_KEY
  )
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  img <- tryCatch({
    utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
    png::readPNG(tmp)
  }, error = function(e) NULL)

  if (is.null(img)) {
    img <- array(1, dim = c(256, 256, 3))
  } else if (length(dim(img)) == 2) {
    img <- array(rep(img, 3), dim = c(dim(img), 3))
  } else if (dim(img)[3] >= 3) {
    img <- img[, , 1:3, drop = FALSE]
  }
  assign(key, img, envir = .cha_tile_cache)
  img
}

#' Download and stitch the CARTO basemap tiles covering a lon/lat bbox
#' (as returned by `combined_bbox()`) into a single RGB raster, returning it
#' together with its extent in EPSG:3857 (meters) so it can be drawn with
#' `ggplot2::annotation_raster()` alongside layers projected to EPSG:3857.
fetch_basemap_mosaic <- function(bbox, max_tiles_across = 5, pad_frac = 0.02) {
  pad_x <- max((bbox[["xmax"]] - bbox[["xmin"]]) * pad_frac, 1e-4)
  pad_y <- max((bbox[["ymax"]] - bbox[["ymin"]]) * pad_frac, 1e-4)
  lon_min <- max(bbox[["xmin"]] - pad_x, -180)
  lon_max <- min(bbox[["xmax"]] + pad_x, 180)
  lat_min <- max(bbox[["ymin"]] - pad_y, -85.05)
  lat_max <- min(bbox[["ymax"]] + pad_y, 85.05)

  zoom <- 18
  repeat {
    x_left  <- cha_lon_to_tile_x(lon_min, zoom)
    x_right <- cha_lon_to_tile_x(lon_max, zoom)
    y_top   <- cha_lat_to_tile_y(lat_max, zoom)
    y_bot   <- cha_lat_to_tile_y(lat_min, zoom)
    if (max(x_right - x_left + 1, y_bot - y_top + 1) <= max_tiles_across || zoom <= 1) break
    zoom <- zoom - 1
  }

  n_x <- x_right - x_left + 1
  n_y <- y_bot - y_top + 1
  mosaic <- array(1, dim = c(n_y * 256, n_x * 256, 3))
  for (ix in seq(x_left, x_right)) {
    for (iy in seq(y_top, y_bot)) {
      tile <- cha_fetch_tile(zoom, ix, iy)
      row0 <- (iy - y_top) * 256
      col0 <- (ix - x_left) * 256
      mosaic[(row0 + 1):(row0 + 256), (col0 + 1):(col0 + 256), ] <- tile
    }
  }

  corners <- sf::st_sfc(
    sf::st_point(c(cha_tile_x_to_lon(x_left, zoom), cha_tile_y_to_lat(y_top, zoom))),
    sf::st_point(c(cha_tile_x_to_lon(x_right + 1, zoom), cha_tile_y_to_lat(y_bot + 1, zoom))),
    crs = 4326
  )
  corners_3857 <- sf::st_coordinates(sf::st_transform(corners, 3857))

  list(
    raster = mosaic,
    xmin = corners_3857[1, "X"], xmax = corners_3857[2, "X"],
    ymax = corners_3857[1, "Y"], ymin = corners_3857[2, "Y"]
  )
}

#' Pick a "nice" round scale-bar length (1/2/5 x a power of ten) not
#' exceeding `target_km`.
cha_nice_scale_km <- function(target_km) {
  if (!is.finite(target_km) || target_km <= 0) return(1)
  exponent <- floor(log10(target_km))
  candidates <- as.vector(outer(c(1, 2, 5), 10^seq(exponent - 1, exponent + 1)))
  candidates <- sort(candidates[candidates <= target_km])
  if (length(candidates) == 0) min(10^(exponent - 1) * c(1, 2, 5)) else max(candidates)
}

#' Render a static ggplot2 map (CARTO basemap + uploaded/analysis layers) for
#' PNG export. `bbox` is a `combined_bbox()`-style lon/lat bbox -- the total
#' extent of the uploaded/analysis layers. `layers` is a list of lists, each
#' with:
#'   data      - an sf object (any CRS)
#'   label     - legend label
#'   color     - hex outline/point color
#'   geom      - "polygon" (default) or "point"
#'   fill      - set to FALSE to draw an unfilled outline
#'   alpha     - fill opacity for polygons (default 0.15)
#'   linewidth - outline width (default 0.8)
#'   linetype  - "solid" (default) or "dashed"
#'   size      - point size (default 1.6)
#'
#' Map furniture (north arrow, scale bar, legend) is drawn in dedicated
#' margin bands reserved above/below `bbox`, not on top of it, so it can
#' never cover the actual data -- the bands are sized to the number of
#' legend entries, and a runtime check (below) verifies each element's
#' anchor point stays clear of the data extent before the plot is returned.
#' Widen a lon/lat bbox horizontally (preserving its vertical extent and true
#' geographic scale) so its projected (EPSG:3857) width/height ratio is at
#' least `target_ratio`. Used so static map exports come out landscape even
#' when the study area itself is a narrow north-south corridor -- the extra
#' width shows more real basemap context on the sides rather than blank
#' padding, so nothing is stretched or distorted.
cha_widen_bbox_to_aspect <- function(bbox, target_ratio) {
  corners <- sf::st_sfc(
    sf::st_point(c(bbox[["xmin"]], bbox[["ymin"]])),
    sf::st_point(c(bbox[["xmax"]], bbox[["ymax"]])),
    crs = 4326
  )
  m <- sf::st_coordinates(sf::st_transform(corners, 3857))
  width_m <- m[2, "X"] - m[1, "X"]
  height_m <- m[2, "Y"] - m[1, "Y"]
  if (width_m >= height_m * target_ratio) return(bbox)

  extra_width_m <- height_m * target_ratio - width_m
  lat_center <- mean(c(bbox[["ymin"]], bbox[["ymax"]]))
  meters_per_deg_lon <- 111320 * cos(lat_center * pi / 180)
  extra_lon <- (extra_width_m / 2) / meters_per_deg_lon

  bbox["xmin"] <- bbox[["xmin"]] - extra_lon
  bbox["xmax"] <- bbox[["xmax"]] + extra_lon
  bbox
}

render_cha_static_map <- function(bbox, layers) {
  # Reserve bands (as fractions of the data extent) above/below `bbox` for
  # the north arrow and for the scale bar + legend; the bottom band grows
  # with the number of legend rows so a longer legend still fits.
  n_legend <- length(layers)
  bottom_frac <- min(0.16 + 0.05 * n_legend, 0.6)
  top_frac <- 0.18
  data_w <- bbox[["xmax"]] - bbox[["xmin"]]
  data_h <- bbox[["ymax"]] - bbox[["ymin"]]
  expanded_bbox <- c(
    xmin = bbox[["xmin"]] - data_w * 0.04, xmax = bbox[["xmax"]] + data_w * 0.08,
    ymin = bbox[["ymin"]] - data_h * bottom_frac, ymax = bbox[["ymax"]] + data_h * top_frac
  )
  # Match the 9x6in (3:2) canvas used by every ggsave() call for the map
  # downloads, so the plotted extent fills the whole landscape canvas.
  expanded_bbox <- cha_widen_bbox_to_aspect(expanded_bbox, target_ratio = 3 / 2)

  mosaic <- fetch_basemap_mosaic(expanded_bbox)
  w <- mosaic$xmax - mosaic$xmin
  h <- mosaic$ymax - mosaic$ymin

  # The actual data extent (not the expanded/padded one above), in the same
  # EPSG:3857 meters as the mosaic, is what furniture must stay clear of.
  data_corners <- sf::st_sfc(
    sf::st_point(c(bbox[["xmin"]], bbox[["ymin"]])),
    sf::st_point(c(bbox[["xmax"]], bbox[["ymax"]])),
    crs = 4326
  )
  data_3857 <- sf::st_coordinates(sf::st_transform(data_corners, 3857))
  core_ymin <- data_3857[1, "Y"]
  core_ymax <- data_3857[2, "Y"]

  p <- ggplot2::ggplot() +
    ggplot2::annotation_raster(
      mosaic$raster, mosaic$xmin, mosaic$xmax, mosaic$ymin, mosaic$ymax, interpolate = TRUE
    )

  for (layer in layers) {
    data_3857 <- sf::st_transform(sf::st_make_valid(layer$data), 3857)
    if (identical(layer$geom, "point")) {
      p <- p + ggplot2::geom_sf(
        data = data_3857, color = layer$color,
        size = if (is.null(layer$size)) 1.6 else layer$size,
        inherit.aes = FALSE
      )
    } else {
      fill_color <- if (isFALSE(layer$fill)) {
        NA
      } else {
        scales::alpha(layer$color, if (is.null(layer$alpha)) 0.15 else layer$alpha)
      }
      p <- p + ggplot2::geom_sf(
        data = data_3857, color = layer$color, fill = fill_color,
        linewidth = if (is.null(layer$linewidth)) 0.8 else layer$linewidth,
        linetype = if (is.null(layer$linetype)) "solid" else layer$linetype,
        inherit.aes = FALSE
      )
    }
  }

  # Scale bar (bottom-left), placed within the reserved bottom band (between
  # the mosaic's bottom edge and the actual data extent) -- corrected for Web
  # Mercator's latitude-dependent distortion so the label reflects true
  # ground distance.
  bottom_band_h <- core_ymin - mosaic$ymin
  lat_center <- mean(c(bbox[["ymin"]], bbox[["ymax"]]))
  merc_to_true <- cos(lat_center * pi / 180)
  bar_km <- cha_nice_scale_km(w * merc_to_true / 1000 * 0.25)
  bar_len <- bar_km * 1000 / merc_to_true
  bar_x0 <- mosaic$xmin + w * 0.06
  bar_y0 <- mosaic$ymin + bottom_band_h * 0.3
  bar_h <- bottom_band_h * 0.12
  bar_segments <- data.frame(
    xmin = c(bar_x0, bar_x0 + bar_len / 2),
    xmax = c(bar_x0 + bar_len / 2, bar_x0 + bar_len),
    ymin = bar_y0, ymax = bar_y0 + bar_h,
    fill = c("black", "white")
  )

  # North arrow (top-left), placed within the reserved top band (between the
  # actual data extent and the mosaic's top edge).
  top_band_h <- mosaic$ymax - core_ymax
  arrow_h <- top_band_h * 0.275
  arrow_w <- arrow_h * 0.6
  apex_x <- mosaic$xmin + w * 0.08
  apex_y <- mosaic$ymax - top_band_h * 0.25
  arrow_triangle <- data.frame(
    x = c(apex_x, apex_x - arrow_w / 2, apex_x + arrow_w / 2),
    y = c(apex_y, apex_y - arrow_h, apex_y - arrow_h)
  )

  # Legend (bottom-right), anchored within the same reserved bottom band as
  # the scale bar; `legend.position` below is expressed as a fraction of the
  # full (mosaic) panel, so the anchor is converted from data coordinates.
  legend_y_frac <- (bottom_band_h * 0.5) / h

  # Verify the furniture anchors actually sit outside the data extent before
  # returning the plot -- this is what guarantees (and checks) that the
  # legend/scale bar/north arrow never overlap the uploaded/analysis layers.
  stopifnot(
    "scale bar overlaps the data extent" = bar_y0 + bar_h <= core_ymin,
    "north arrow overlaps the data extent" = apex_y - arrow_h >= core_ymax,
    "legend anchor overlaps the data extent" = mosaic$ymin + legend_y_frac * h <= core_ymin
  )

  legend_labels <- vapply(layers, function(l) l$label, character(1))
  legend_colors <- stats::setNames(vapply(layers, function(l) l$color, character(1)), legend_labels)
  legend_df <- data.frame(x = mosaic$xmin, y = mosaic$ymin, Layer = factor(legend_labels, levels = legend_labels))

  p +
    # Background plaques so the scale bar / north arrow stay legible over any
    # basemap color.
    ggplot2::annotate(
      "rect",
      xmin = bar_x0 - w * 0.02, xmax = bar_x0 + bar_len + w * 0.02,
      ymin = bar_y0 - h * 0.015, ymax = bar_y0 + bar_h + h * 0.05,
      fill = "white", alpha = 0.75, color = NA
    ) +
    ggplot2::annotate(
      "rect",
      xmin = apex_x - arrow_w * 0.9, xmax = apex_x + arrow_w * 0.9,
      ymin = apex_y - arrow_h - h * 0.01, ymax = apex_y + h * 0.05,
      fill = "white", alpha = 0.75, color = NA
    ) +
    ggplot2::geom_rect(
      data = bar_segments,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
      color = "black", linewidth = 0.3, inherit.aes = FALSE, show.legend = FALSE
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::annotate("text", x = bar_x0, y = bar_y0 + bar_h + h * 0.015, label = "0", size = 3, vjust = 0) +
    ggplot2::annotate(
      "text", x = bar_x0 + bar_len, y = bar_y0 + bar_h + h * 0.015,
      label = paste0(bar_km, " km"), size = 3, vjust = 0
    ) +
    ggplot2::geom_polygon(
      data = arrow_triangle, ggplot2::aes(x = x, y = y),
      fill = "black", color = "black", inherit.aes = FALSE
    ) +
    ggplot2::annotate("text", x = apex_x, y = apex_y + h * 0.02, label = "N", fontface = "bold", size = 2) +
    ggplot2::geom_point(
      data = legend_df, ggplot2::aes(x = x, y = y, color = Layer),
      alpha = 0, inherit.aes = FALSE, show.legend = TRUE
    ) +
    ggplot2::scale_color_manual(
      values = legend_colors, name = NULL,
      guide = ggplot2::guide_legend(override.aes = list(alpha = 1, size = 4, shape = 15))
    ) +
    ggplot2::coord_sf(
      xlim = c(mosaic$xmin, mosaic$xmax), ylim = c(mosaic$ymin, mosaic$ymax),
      expand = FALSE, datum = NA
    ) +
    ggplot2::theme_void(base_size = 13) +
    ggplot2::theme(
      legend.position = c(0.98, legend_y_frac),
      legend.justification = c(1, 0.5),
      legend.background = ggplot2::element_rect(fill = scales::alpha("white", 0.85), color = "grey50", linewidth = 0.3),
      legend.margin = ggplot2::margin(6, 8, 6, 8),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA)
    )
}

#' Add a button below a Leaflet widget that downloads a server-rendered
#' static map (see `render_cha_static_map()`) as a PNG.
map_download_button <- function(ns, output_id = "download_map") {
  shiny::downloadButton(
    ns(output_id), "Download map (PNG)",
    icon = shiny::icon("download"), class = "btn-default",
    style = "margin-top: 10px;"
  )
}

#' Read a vector layer uploaded through a shiny fileInput.
#'
#' Supports: a zipped vector file of any kind (.zip -- shapefile preferred if
#' present, otherwise the first recognizable vector file inside), the raw
#' shapefile components (.shp/.shx/.dbf/.prj selected together), or any other
#' single-file vector format that GDAL/sf can read (.gpkg, .geojson, .json,
#' .kml, .gml, .tab, .sqlite, etc.) -- the file extension isn't restricted to
#' a fixed list; sf/GDAL is left to detect the driver, with a clear error
#' surfaced if the format isn't supported.
#'
#' @param file_info the `input$<id>` data.frame produced by `fileInput()`
read_vector_input <- function(file_info) {
  shiny::req(file_info)
  exts <- tolower(tools::file_ext(file_info$name))

  if (all(exts == "zip")) {
    tmp_dir <- tempfile()
    dir.create(tmp_dir)
    utils::unzip(file_info$datapath[1], exdir = tmp_dir)
    candidates <- list.files(tmp_dir, full.names = TRUE, recursive = TRUE)
    shp <- candidates[grepl("\\.shp$", candidates, ignore.case = TRUE)]
    if (length(shp) >= 1) {
      res <- tryCatch(sf::st_read(shp[1], quiet = TRUE), error = function(e) e)
      shiny::validate(shiny::need(
        !inherits(res, "error"),
        sprintf("Could not read the shapefile inside the uploaded zip: %s", if (inherits(res, "error")) conditionMessage(res) else "")
      ))
      return(res)
    }
    other <- candidates[grepl("\\.(gpkg|geojson|json|kml|gml|tab|sqlite)$", candidates, ignore.case = TRUE)]
    shiny::validate(shiny::need(length(other) >= 1, "No recognizable vector file found inside the uploaded zip."))
    res <- tryCatch(sf::st_read(other[1], quiet = TRUE), error = function(e) e)
    shiny::validate(shiny::need(
      !inherits(res, "error"),
      sprintf("Could not read the vector file inside the uploaded zip: %s", if (inherits(res, "error")) conditionMessage(res) else "")
    ))
    return(res)
  }

  if (any(exts == "shp")) {
    required <- c("shp", "shx", "dbf")
    missing_req <- setdiff(required, exts)
    shiny::validate(shiny::need(
      length(missing_req) == 0,
      sprintf(
        "Missing required shapefile component(s): %s. Select the .shp, .shx and .dbf files together (and .prj if available).",
        paste0(".", missing_req, collapse = ", ")
      )
    ))
    tmp_dir <- tempfile()
    dir.create(tmp_dir)
    file.copy(file_info$datapath, file.path(tmp_dir, file_info$name))
    shp <- file.path(tmp_dir, file_info$name[exts == "shp"][1])
    res <- tryCatch(sf::st_read(shp, quiet = TRUE), error = function(e) e)
    shiny::validate(shiny::need(
      !inherits(res, "error"),
      sprintf("Could not read the uploaded shapefile: %s", if (inherits(res, "error")) conditionMessage(res) else "")
    ))
    return(res)
  }

  # Any other single-file vector format -- let sf/GDAL try to read it and
  # surface a clear error if the format/driver isn't supported.
  res <- tryCatch(sf::st_read(file_info$datapath[1], quiet = TRUE), error = function(e) e)
  shiny::validate(shiny::need(
    !inherits(res, "error"),
    sprintf(
      "Could not read '.%s' file as a vector layer%s",
      exts[1],
      if (inherits(res, "error")) paste0(": ", conditionMessage(res)) else "."
    )
  ))
  res
}

#' Read an occurrence points table (e.g. a GBIF CSV export) into an sf object.
read_points_csv <- function(path, lon_col, lat_col, crs = 4326) {
  df <- readr::read_csv(path, show_col_types = FALSE)
  shiny::validate(shiny::need(
    all(c(lon_col, lat_col) %in% names(df)),
    "Selected longitude/latitude columns were not found in the uploaded file."
  ))
  df <- df[!is.na(df[[lon_col]]) & !is.na(df[[lat_col]]), ]
  shiny::validate(shiny::need(nrow(df) > 0, "No valid coordinate records found after removing missing values."))
  sf::st_as_sf(df, coords = c(lon_col, lat_col), crs = crs, remove = FALSE)
}

#' Does an uploaded `input$<id>` file (from fileInput) look like a CSV/TSV
#' table, as opposed to a shapefile/vector file?
is_tabular_points_upload <- function(file_info) {
  shiny::req(file_info)
  all(tolower(tools::file_ext(file_info$name)) %in% c("csv", "tsv"))
}

#' Read an occurrence points upload -- either a CSV/TSV table (using the
#' chosen longitude/latitude columns) or a point shapefile/vector file
#' (.shp + sidecar files, .zip, .gpkg, .geojson) -- into an sf point object.
read_points_file <- function(file_info, lon_col = NULL, lat_col = NULL, crs = 4326) {
  shiny::req(file_info)

  if (is_tabular_points_upload(file_info)) {
    shiny::validate(shiny::need(
      !is.null(lon_col) && !is.null(lat_col),
      "Select the longitude and latitude columns for the uploaded table."
    ))
    return(read_points_csv(file_info$datapath[1], lon_col, lat_col, crs = crs))
  }

  pts <- read_vector_input(file_info)

  n_before <- nrow(pts)
  pts <- pts[!sf::st_is_empty(pts), ]
  n_dropped <- n_before - nrow(pts)
  shiny::validate(shiny::need(
    nrow(pts) > 0,
    "All records in the uploaded file have empty/missing geometry -- no valid occurrence points found."
  ))
  if (n_dropped > 0) {
    shiny::showNotification(
      sprintf("%d of %d record(s) had empty/missing coordinates and were skipped.", n_dropped, n_before),
      type = "warning"
    )
  }

  geom_types <- as.character(unique(sf::st_geometry_type(pts)))
  shiny::validate(shiny::need(
    all(geom_types %in% c("POINT", "MULTIPOINT")),
    sprintf(
      "Uploaded file has %s geometries; occurrence records must be point data (upload a CSV/TSV table or a point shapefile instead).",
      paste(geom_types, collapse = ", ")
    )
  ))
  sf::st_transform(pts, crs)
}

#' Build a minimum convex polygon (convex hull) around a set of points.
make_convex_hull <- function(points) {
  sf::st_convex_hull(sf::st_union(sf::st_geometry(points)))
}

#' Combine the bounding boxes of one or more sf/sfc objects (NULLs ignored)
#' into a single bbox covering all of them, in lon/lat (EPSG:4326). Used to
#' fit the leaflet map view to both the EAAA and an uploaded layer, even when
#' the uploaded layer falls outside (or only partially inside) the EAAA.
#' Returns NULL (instead of erroring) when no layers are supplied, since this
#' is called from plain `observe()` blocks rather than a render/reactive
#' context where `shiny::validate()` would normally surface a message.
combined_bbox <- function(...) {
  layers <- Filter(Negate(is.null), list(...))
  if (length(layers) == 0) return(NULL)
  boxes <- lapply(layers, function(x) sf::st_bbox(sf::st_transform(x, 4326)))
  c(
    xmin = min(vapply(boxes, `[[`, numeric(1), "xmin")),
    ymin = min(vapply(boxes, `[[`, numeric(1), "ymin")),
    xmax = max(vapply(boxes, `[[`, numeric(1), "xmax")),
    ymax = max(vapply(boxes, `[[`, numeric(1), "ymax"))
  )
}

#' Refresh a leaflet legend from a named character vector of
#' `label = "#color"` pairs (e.g. `c(EAAA = "#e31a1c")`). Clears any existing
#' legend first; passing a zero-length vector just removes it.
update_map_legend <- function(session, map_id, entries) {
  leaflet::leafletProxy(map_id, session = session) %>% leaflet::clearControls()
  if (length(entries) == 0) return(invisible())
  leaflet::leafletProxy(map_id, session = session) %>%
    leaflet::addLegend(
      position = "bottomright", colors = unname(entries), labels = names(entries),
      opacity = 0.8, title = "Legend"
    )
}

#' Count how many points fall inside a polygon and the resulting percentage.
points_in_polygon <- function(points, poly) {
  poly <- sf::st_transform(sf::st_make_valid(poly), sf::st_crs(points))
  inside <- lengths(sf::st_intersects(points, sf::st_union(sf::st_geometry(poly)))) > 0
  list(
    n_total = nrow(points),
    n_inside = sum(inside),
    pct_inside = if (nrow(points) > 0) 100 * sum(inside) / nrow(points) else NA_real_
  )
}

#' Standard WGS84 UTM zone EPSG code for a lon/lat location (e.g. 32718 for
#' UTM zone 18S). UTM is used as the app's working projected CRS because,
#' unlike an ad-hoc local projection, it is a standard, reviewer-recognizable
#' CRS and keeps distortion low across a single study area.
utm_epsg_for_lonlat <- function(lon, lat) {
  zone <- floor((lon + 180) / 6) %% 60 + 1
  base <- if (lat >= 0) 32600 else 32700
  as.integer(base + zone)
}

#' Summarize a layer's native CRS -- whether it's geographic (longitude/
#' latitude) or already projected, its name, and its approximate location (in
#' WGS84) -- so the app can tell the user what was detected on upload. Uses
#' the layer's bounding box rather than its full geometry, so this stays fast
#' even for huge and/or invalid multipart layers.
describe_layer_crs <- function(x) {
  crs <- sf::st_crs(x)
  is_geo <- tryCatch(sf::st_is_longlat(x), error = function(e) NA)
  center <- tryCatch({
    bbox_center_native <- sf::st_centroid(sf::st_as_sfc(sf::st_bbox(x)))
    sf::st_coordinates(sf::st_transform(bbox_center_native, 4326))
  }, error = function(e) matrix(c(NA_real_, NA_real_), ncol = 2))
  list(
    name = if (is.na(crs)) NA_character_ else crs$Name,
    is_geographic = isTRUE(is_geo),
    centroid_lon = center[1, 1], centroid_lat = center[1, 2]
  )
}

#' One-line, user-facing summary of `describe_layer_crs()`, noting whether
#' the uploaded layer was geographic or projected and where it is located.
crs_status_message <- function(x) {
  info <- describe_layer_crs(x)
  kind <- if (is.na(info$is_geographic)) {
    "CRS could not be determined"
  } else if (info$is_geographic) {
    "geographic (longitude/latitude) CRS"
  } else {
    "already a projected CRS"
  }
  layer_name <- if (is.na(info$name)) "unspecified" else info$name
  sprintf(
    "Detected input CRS: %s (%s). Location: %.3f, %.3f.",
    layer_name, kind, info$centroid_lon, info$centroid_lat
  )
}

#' Pick the working projected CRS (a UTM zone) for the whole assessment,
#' based on the EAAA's location, so every overlap/area calculation in the
#' app uses one consistent, country-appropriate CRS instead of an ad-hoc
#' projection recomputed per call.
pick_study_crs <- function(eaaa) {
  cen <- sf::st_coordinates(sf::st_centroid(sf::st_union(sf::st_transform(sf::st_make_valid(eaaa), 4326))))
  utm_epsg_for_lonlat(cen[1, "X"], cen[1, "Y"])
}

#' Calculate overlap area/percentages between a target layer and the EAAA,
#' in the shared study CRS (`crs`, see `pick_study_crs()`). Assumes both
#' layers are country/region-scale (the app's normal use case); if a layer
#' turns out to be far larger (e.g. an unclipped national/global dataset),
#' it can reproject to a near-zero area outside the study CRS's UTM zone, so
#' that case is caught explicitly below rather than silently reported.
calc_overlap <- function(target, eaaa, crs) {
  target <- sf::st_transform(sf::st_make_valid(target), 4326)
  eaaa   <- sf::st_transform(sf::st_make_valid(eaaa), 4326)

  target_ea <- sf::st_transform(target, crs)
  eaaa_ea   <- sf::st_transform(eaaa, crs)

  inter <- suppressWarnings(
    sf::st_intersection(sf::st_union(sf::st_geometry(target_ea)), sf::st_union(sf::st_geometry(eaaa_ea)))
  )

  area_overlap <- if (length(inter) == 0) 0 else as.numeric(sum(sf::st_area(inter)))
  area_target  <- as.numeric(sum(sf::st_area(target_ea)))
  area_eaaa    <- as.numeric(sum(sf::st_area(eaaa_ea)))

  shiny::validate(shiny::need(
    area_target > 0,
    paste(
      "The uploaded layer's area came out as zero once projected to the study",
      "area's UTM zone. This usually means its real extent is much larger than",
      "the study area (e.g. an unclipped national/global dataset) -- clip it to",
      "the study region and re-upload."
    )
  ))

  list(
    area_overlap_km2 = area_overlap / 1e6,
    area_target_km2  = area_target / 1e6,
    area_eaaa_km2    = area_eaaa / 1e6,
    pct_of_target    = if (area_target > 0) 100 * area_overlap / area_target else NA_real_,
    pct_of_eaaa      = if (area_eaaa > 0) 100 * area_overlap / area_eaaa else NA_real_
  )
}

#' Build a results data.frame comparing a computed percentage against a
#' criterion's tier thresholds (as configured in `THRESHOLDS` in global.R).
#' `area_ha`, when supplied, is included as an "Overlap area (ha)" column so
#' results can be read alongside the underlying area, not just the percentage.
build_threshold_table <- function(pct, thresholds, area_ha = NA_real_) {
  rows <- lapply(thresholds, function(th) {
    data.frame(
      Tier = th$label,
      `Threshold (%)` = th$pct,
      `Computed (%)` = round(pct, 2),
      `Overlap area (ha)` = round(area_ha, 2),
      Triggered = isTRUE(pct >= th$pct),
      check.names = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Highlight results-table rows by whether they trigger Critical Habitat,
#' based on a logical `Triggered` column (a no-op if that column is absent,
#' e.g. the Vulnerable-species rows which carry no threshold comparison).
style_threshold_table <- function(dt, df) {
  if (!"Triggered" %in% names(df)) return(dt)
  DT::formatStyle(
    dt, "Triggered", target = "row",
    backgroundColor = DT::styleEqual(c(TRUE, FALSE), c("#f8d7da", "#d4edda")),
    fontWeight = DT::styleEqual(c(TRUE, FALSE), c("bold", "normal"))
  )
}
