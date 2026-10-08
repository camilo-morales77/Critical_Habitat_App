setwd("C:/Users/Camilo.Morales/OneDrive - Arup/Documents/Scripts/APP_CHA")
source("global.R")
invisible(lapply(list.files("R", pattern = "[.]R$", full.names = TRUE), source))

eaaa_path <- list.files("C:/Users/Camilo.Morales/OneDrive - Arup/Documents", pattern = "^Polygons\\.shp$", recursive = TRUE, full.names = TRUE)
eco_path  <- list.files("C:/Users/Camilo.Morales/OneDrive - Arup/Documents", pattern = "^Rainforest_Dissolved\\.shp$", recursive = TRUE, full.names = TRUE)

eaaa <- sf::st_read(eaaa_path, quiet = TRUE)
eco  <- sf::st_read(eco_path, quiet = TRUE)
study_crs <- pick_study_crs(eaaa)
eaaa_4326 <- sf::st_transform(sf::st_make_valid(eaaa), 4326)

cat("=== Case A: normal, country-scale layer (eco clipped to a 50km buffer around EAAA) ===\n")
buffer_native <- suppressWarnings(sf::st_transform(sf::st_buffer(sf::st_transform(eaaa_4326, study_crs), 50000), sf::st_crs(eco)))
eco_local_raw <- suppressWarnings(sf::st_crop(eco, sf::st_bbox(buffer_native)))
t0 <- Sys.time()
ov_local <- calc_overlap(eco_local_raw, eaaa, study_crs)
cat("time:", as.numeric(Sys.time() - t0), "s\n")
print(ov_local)

cat("\n=== Case B: pathological global (uncropped) layer -- should now fail clearly ===\n")
result <- tryCatch({
  calc_overlap(eco, eaaa, study_crs)
}, error = function(e) e, condition = function(c) c)
cat("Class:", paste(class(result), collapse = ", "), "\n")
cat("Message:", conditionMessage(result), "\n")
