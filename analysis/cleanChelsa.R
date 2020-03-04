#' Clean Chelsa Climate Layers
#'
#' This R script prepares climate layers by:
#'   - cropping and masking with study area
#'   - upscaling resolution
#'   - projecting in the Lambert CRS
#' It also selects countries from study area and project the shapefile
#'
#' @author Nicolas Casajus, \email{nicolas.casajus@@fondationbiodiversite.fr}
#' @date 2020/03/03




## Import Study Area ----

study_area <- sf::st_read(
  dsn    = file.path(
    path_boundary_data,
    "study_area",
    "Med_Study_area_with_countries_and_islands_111017.shp"
  ),
  quiet  = TRUE
)


## Import Sampling Efforts ----

sampling <- read.csv2(
  file.path(
    path_species_data,
    "woodiv_country_sampling_effort_table.csv"
  )
)

countries <- as.character(sampling[sampling[ , "study_area"] == 1, "NAME_ENGLI"])


## Crop Study Area ----

study_area <- study_area[study_area$"NAME_ENGLI" %in% countries, ]


## Import Climate Rasters ----

climate <- raster::stack(
  x = file.path(
    path_climate_data,
    horizon,
    paste0("CHELSA_bio10_", varnames, ".tif")
  )
)


## Crop and Mask Climate Rasters ----

cat("\n", emo::ji("check"), "Cropping Chelsa rasters")

climate <- mask(
  crop(
    climate,
    extent(study_area)
  ),
  study_area
)


## Upscale Climate Rasters ----

climate <- aggregate(climate, fac = 10)


## Project Study Area ----

cat("\n", emo::ji("check"), "Projecting study area")

study_area <- sf::st_transform(
  x    = study_area,
  crs  = lambert
)


## Project Climate Rasters ----

cat("\n", emo::ji("check"), "Projecting Chelsa rasters")

climate <- projectRaster(from = climate, crs = lambert)


## Export Layers ----

cat("\n", emo::ji("check"), "Saving cropped Chelsa rasters")

dir.create(path = file.path("output", "climate"), showWarnings = FALSE)

for (layer in 1:nlayers(climate)) {

  writeRaster(
    x         = subset(climate, layer),
    filename  = file.path(
      "output",
      "climate",
      paste0(names(climate)[layer], "_cropped.tif")
    ),
    format    = "GTiff",
    overwrite = TRUE
  )
}

dir.create(path = file.path("output", "study_area"), showWarnings = FALSE)

sf::st_write(
  obj          = study_area,
  dsn          = file.path("output", "study_area"),
  layer        = "Mediterranean_area_cropped",
  driver       = "ESRI Shapefile",
  delete_layer = TRUE
)

cat("\n")
