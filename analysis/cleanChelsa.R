#' @title Clean Chelsa climate layers
#'
#' @description
#' This R script...
#'
#' @author Nicolas Casajus, \email{nicolas.casajus@@fondationbiodiversite.fr}
#'
#' @date 16/10/2019
#'



#' ---------------------------------------------------------------------------- @ImportStudyArea

study_area <- sf::st_read(
  dsn    = file.path(
    path_boundary_data,
    "study_area",
    "Med_Study_area_with_countries_and_islands_111017.shp"
  ),
  quiet  = TRUE
)


#' ---------------------------------------------------------------------------- @ImportSamplingEfforts

sampling <- read.csv2(
  file.path(
    path_species_data,
    "woodiv_country_sampling_effort_table.csv"
  )
)

countries <- as.character(sampling[sampling[ , "study_area"] == 1, "NAME_ENGLI"])


#' ---------------------------------------------------------------------------- @CropStudyArea

study_area <- study_area[study_area$"NAME_ENGLI" %in% countries, ]


#' ---------------------------------------------------------------------------- @ImportClimateRasters

climate <- raster::stack(
  x = file.path(
    path_climate_data,
    horizon,
    paste0("CHELSA_bio10_", varnames, ".tif")
  )
)


#' ---------------------------------------------------------------------------- @CropClimateRasters

cat("\n", emo::ji("check"), "Cropping Chelsa rasters")

climate <- mask(
  crop(
    climate,
    extent(study_area)
  ),
  study_area
)


#' ---------------------------------------------------------------------------- @ProjectStudyArea

cat("\n", emo::ji("check"), "Projecting study area")

study_area <- sf::st_transform(
  x    = study_area,
  crs  = lambert
)


#' ---------------------------------------------------------------------------- @ProjectClimateRasters

cat("\n", emo::ji("check"), "Projecting Chelsa rasters")

climate <- projectRaster(from = climate, crs = lambert)


#' ---------------------------------------------------------------------------- @ExportLayers

cat("\n", emo::ji("check"), "Saving cropped Chelsa rasters")

save(climate, file = file.path("output", paste0("CHELSA_biovars_cropped")))
save(study_area, file = file.path("output", paste0("Mediterranean_area_cropped")))

cat("\n")
